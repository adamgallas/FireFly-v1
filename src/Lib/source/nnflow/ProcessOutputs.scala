package nnflow

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object ProcessOutputs {
  def apply[T <: Data](
                        source: Stream[T],
                        width: UInt, height: UInt, frames: UInt, enablePooling: Bool,
                        bufferDepth: Int, op: (T, T) => T
                      ) = {

    val process = new ProcessOutputs(
      dataType = source.payloadType,
      bufferDepth = bufferDepth,
      op = op
    )
    process.io.width := width.resized
    process.io.height := height.resized
    process.io.frames := frames.resized
    process.io.enablePooling := enablePooling
    process.io.source << source
    (process.io.dest, process.io.done)
  }
}

class ProcessOutputs[T <: Data](dataType: HardType[T], bufferDepth: Int, op: (T, T) => T) extends Component {
  val io = new Bundle {
    val width = in UInt (log2Up(bufferDepth) bits)
    val height = in UInt (log2Up(bufferDepth) bits)
    val frames = in UInt (12 bits)
    val enablePooling = in Bool()
    val done = out Bool()
    val source = slave(Stream(dataType))
    val dest = master(Stream(dataType))
  }
  noIoPrefix()
  val pool = video.VideoPooling(dataType, bufferDepth, op)
  pool.io.enable := io.enablePooling
  pool.io.videoIn << video.VideoPackSignalGen(io.source, io.width, io.height, io.frames)
  io.dest.arbitrationFrom(pool.io.videoOut)
  io.dest.payload := pool.io.videoOut.getPixel
  io.done := pool.io.videoOut.endOfPack
}
