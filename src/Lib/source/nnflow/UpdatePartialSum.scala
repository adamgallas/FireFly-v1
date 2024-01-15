package nnflow

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class UpdatePartialSum(widthOfSum: Int, widthOfPartial: Int, channels: Int, signed: Boolean) extends Component {
  val io = new Bundle {
    val numOfIsPerO = in UInt (12 bits)
    val offset = slave(Stream(Vec(Bits(widthOfSum bits), channels)))
    val extract = slave(Stream(Fragment(utils.Linked(Vec(Bits(widthOfPartial bits), channels), Vec(Bits(widthOfSum bits), channels)))))
    val update = master(Stream(Fragment(Vec(Bits(widthOfPartial bits), channels))))
    val results = master(Stream(Fragment(Vec(Bits(widthOfPartial bits), channels))))
  }
  noIoPrefix()
  val cnt = Counter(12 bits, inc = io.extract.fire && io.extract.last)
  val first = cnt === 0
  val last = cnt === io.numOfIsPerO
  when(last && cnt.willIncrement)(cnt.clear())

  val fifo = fifos.StreamFifoHighPerf(TupleBundle2(Fragment(Vec(Bits(widthOfPartial bits), channels)), Bool()), 8)

  val os = RegNext(RegNext(io.offset.payload))
  val lastDly = RegNext(RegNext(last))
  val curr = RegNext(io.extract.linked)
  val prev = RegNext(Mux(first, Vec(B(0, widthOfPartial bits), channels), io.extract.value))
  if (signed) {
    val post = Vec((curr, prev).zipped.map((x, y) => RegNext(x.asSInt + y.asSInt)))
    val postOs = Vec((post, os).zipped.map((x, y) => RegNext(x + y.asSInt.andMask(lastDly))))
    fifo.io.push._1.fragment.assignFromBits(postOs.asBits)
  }
  else {
    val post = Vec((curr, prev).zipped.map((x, y) => RegNext(x.asUInt + y.asUInt)))
    val postOs = Vec((post, os).zipped.map((x, y) => RegNext(x + y.asUInt.andMask(lastDly))))
    fifo.io.push._1.fragment.assignFromBits(postOs.asBits)
  }
  fifo.io.push._1.last := Delay(io.extract.last, 3, init = False)
  fifo.io.push._2 := RegNext(lastDly)
  fifo.io.push.valid := Delay(io.extract.fire, 3, init = False)
  io.extract.ready := fifo.io.pop.ready

  val (toUpdate, toResults) = utils.StreamPath(fifo.io.pop, fifo.io.pop._2)
  io.update.arbitrationFrom(toUpdate)
  io.results.arbitrationFrom(toResults)
  io.update.payload := toUpdate.payload._1
  io.results.payload := toResults._1

  io.offset.ready := fifo.io.push.fire && fifo.io.push._1.last && fifo.io.push._2
}
