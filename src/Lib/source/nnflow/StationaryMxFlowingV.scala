package nnflow

import spinal.core._
import spinal.lib._
import utils._

import scala.language.postfixOps

object StationaryMxFlowingV {
  def apply(
             source: Stream[Fragment[Linked[Vec[Bits], Vec[Vec[Bits]]]]],
             widthOfProduct: Int, widthOfSum: Int,
             SIMD: (Int, Int),
             stages: Int,
             signed: Boolean,
             xop: (Vec[Bits], Vec[Vec[Bits]]) => Vec[Bits],
             aop: Vec[Bits] => Bits
           ) = {

    val MxV = new StationaryMxFlowingV(
      widthOfInput = source.value.head.getWidth,
      widthOfWeight = source.linked.head.head.getWidth,
      widthOfProduct = widthOfProduct,
      widthOfSum = widthOfSum,
      dimension = (source.value.length, source.linked.head.length),
      SIMDIn = SIMD._1,
      SIMDOut = SIMD._2,
      stages = stages,
      signed = signed,
      xop = xop,
      aop = aop
    )

    MxV.io.source << source
    MxV.io.dest
  }
}

class StationaryMxFlowingV(
                                 widthOfInput: Int,
                                 widthOfWeight: Int,
                                 widthOfProduct: Int,
                                 widthOfSum: Int,
                                 dimension: (Int, Int),
                                 SIMDIn: Int,
                                 SIMDOut: Int,
                                 stages: Int,
                                 signed: Boolean,
                                 xop: (Vec[Bits], Vec[Vec[Bits]]) => Vec[Bits],
                                 aop: Vec[Bits] => Bits
                               ) extends Component {

  require(stages < 16)
  val MatrixWidth = dimension._1
  val MatrixHeight = dimension._2

  val io = new Bundle {
    val source = slave(Stream(Fragment(
      Linked(Vec(Bits(widthOfInput bits), MatrixWidth), Vec(Vec(Bits(widthOfWeight bits), MatrixHeight), MatrixWidth)))))
    val dest = master(Stream(Fragment(
      Vec(Bits(widthOfSum bits), MatrixHeight)
    )))
  }
  noIoPrefix()

  val input = Reshape(
    io.source.value,
    Bits(widthOfInput bits),
    (SIMDIn, MatrixWidth / SIMDIn)
  )
  val weight = Reshape(
    io.source.linked,
    Bits(widthOfWeight bits),
    (SIMDOut, MatrixHeight / SIMDOut, SIMDIn, MatrixWidth / SIMDIn)
  )

  val product = Vec(Vec(Vec(Bits(widthOfProduct bits), MatrixWidth / SIMDIn), SIMDOut), MatrixHeight / SIMDOut)
  val merged = Vec(Vec(Bits(widthOfSum bits), SIMDOut), MatrixHeight / SIMDOut)

  for (h <- 0 until MatrixHeight / SIMDOut) {
    for (w <- 0 until MatrixWidth / SIMDIn) {
      val inputSlice = input(w)
      val weightSlice = Vec((0 until SIMDIn).map(weight(w)(_)(h)))
      val outSlice = xop(inputSlice, weightSlice)
      outSlice.zipWithIndex.foreach(x => product(h)(x._2)(w) := x._1)
    }
    for (s <- 0 until SIMDOut) {
      if (signed) merged(h)(s) := aop(product(h)(s)).asSInt.resize(widthOfSum).asBits
      else merged(h)(s) := aop(product(h)(s)).asUInt.resize(widthOfSum).asBits
    }
  }

  val fifo = fifos.StreamFifoHighPerf(Fragment(Vec(Bits(widthOfSum bits), MatrixHeight)), 16)
  fifo.io.push.valid := Delay(io.source.fire, stages, init = False)
  fifo.io.push.last := Delay(io.source.last, stages, init = False)
  fifo.io.push.fragment.assignFromBits(merged.asBits)
  fifo.io.pop >> io.dest
  io.source.ready := io.dest.ready
}
