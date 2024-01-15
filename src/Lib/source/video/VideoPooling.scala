package video

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object VideoPooling {
  def apply[T <: Data](
                        dataType: HardType[T],
                        maxDepth: Int,
                        op: (T, T) => T
                      ) = new VideoPooling(dataType, maxDepth, op)
}

class VideoPooling[T <: Data](
                                dataType: HardType[T],
                                maxDepth: Int,
                                op: (T, T) => T
                              ) extends Component {
  val io = new Bundle {
    val enable = in Bool()
    val videoIn = slave(Stream(VideoPack(dataType)))
    val videoOut = master(Stream(VideoPack(dataType)))
  }
  noIoPrefix()
  val window = SlidingWindowValid(dataType, 2, maxDepth)
  val downSample = VideoDownSample(dataType, even = false)

  val merge = Stream(VideoPack(Vec(dataType, 2)))
  val mergePipe = merge.m2sPipe()
  val bridge = Stream(VideoPack(dataType))
  val bridgePipe = bridge.m2sPipe()

  merge.arbitrationFrom(window.io.videoOut)
  merge.payload.arbitrationSignalFrom(window.io.videoOut)
  merge.frame.line.pixel := Vec(
    op(window.io.videoOut.frame.line.pixel(0)(0), window.io.videoOut.frame.line.pixel(0)(1)),
    op(window.io.videoOut.frame.line.pixel(1)(0), window.io.videoOut.frame.line.pixel(1)(1))
  )

  bridge.arbitrationFrom(mergePipe)
  bridge.payload.arbitrationSignalFrom(mergePipe)
  bridge.frame.line.pixel := op(mergePipe.frame.line.pixel(0), mergePipe.frame.line.pixel(1))

  downSample.io.videoIn << bridgePipe

  utils.StreamPath(io.videoIn,io.enable,(window.io.videoIn,downSample.io.videoOut)).s2mPipe().m2sPipe() >> io.videoOut
}