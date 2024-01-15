package nnflow

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object ProcessInputs {
  def apply[T <: Data](
                        source: Stream[T],
                        width: UInt, height: UInt, frames: UInt,
                        directAdapt: Bool, offset: SInt,
                        factor: Int, bufferDepth: Int, needOffset: Boolean
                      ) = {

    val process = new ProcessInputs(
      dataType = source.payloadType,
      factor = factor,
      bufferDepth = bufferDepth,
      needOffset = needOffset
    )
    process.io.width := width.resized
    process.io.height := height.resized
    process.io.frames := frames.resized
    process.io.offset := offset
    process.io.directAdapt := directAdapt
    process.io.source << source
    process.io.dest
  }
}

class ProcessInputs[T <: Data](dataType: HardType[T], factor: Int, bufferDepth: Int, needOffset: Boolean) extends Component {
  val kernelSize = scala.math.sqrt(factor).toInt
  val io = new Bundle {
    val width = in UInt (log2Up(bufferDepth) bits)
    val height = in UInt (log2Up(bufferDepth) bits)
    val frames = in UInt (12 bits)
    val offset = in SInt (8 bits) default S(0, 8 bits)
    val directAdapt = in Bool()
    val source = slave(Stream(dataType))
    val dest = master(Stream(Fragment(Vec(dataType, factor))))
  }
  noIoPrefix()
  val directSrc = Stream(dataType)
  val directDst = Stream(Fragment(Vec(dataType, factor)))
  val linebufSrc = Stream(dataType)
  val linebufDst = Stream(Fragment(Vec(dataType, factor)))

  val src = Stream(dataType)
  if (needOffset) {
    val split = io.source.payload.asBits.subdivideIn(8 bits).as(Vec(SInt(8 bits), dataType.getBitsWidth / 8))
    val splitos = Vec(split.map(_ - io.offset))
    src << io.source.translateWith(splitos.as(dataType)).m2sPipe()
  }
  else {
    src << io.source
  }

  io.dest << utils.StreamPath(
    src, io.directAdapt,
    (linebufSrc, linebufDst),
    (directSrc, directDst)
  )

  StreamFragmentWidthAdapter(directSrc.addFragmentLast(True), directDst)

  val window = video.SlidingWindowSame(dataType, kernelSize, bufferDepth)
  window.io.videoIn << video.VideoPackSignalGen(linebufSrc, io.width, io.height, io.frames)
  linebufDst.arbitrationFrom(window.io.videoOut)
  linebufDst.fragment.assignFromBits(window.io.videoOut.getPixel.asBits)
  linebufDst.last := window.io.videoOut.frame.endOfFrame
}
