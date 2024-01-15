package video

import spinal.core._
import spinal.lib._

object SlidingWindowValid {
  def apply[T <: Data](dataType: HardType[T], kernelSize: Int, maxWidth: Int) = new SlidingWindowValid(dataType, kernelSize, maxWidth)
}

class SlidingWindowValid[T <: Data](dataType: HardType[T], kernelSize: Int, maxWidth: Int) extends Component {
  val io = new Bundle {
    val videoIn = slave(Stream(VideoPack(dataType)))
    val videoOut = master(Stream(VideoPack(Vec(Vec(dataType, kernelSize), kernelSize))))
  }
  noIoPrefix()
  val line = LineBufferValid(dataType, maxWidth, kernelSize)
  val pixel = PixelBufferValid(Vec(dataType, kernelSize), kernelSize)
  val pixelOut = pixel.io.videoOut

  line.io.videoIn << io.videoIn
  pixel.io.videoIn << line.io.videoOut.s2mPipe().m2sPipe()

  io.videoOut.arbitrationFrom(pixelOut)
  io.videoOut.endOfPack := pixelOut.endOfPack
  io.videoOut.frame.endOfFrame := pixelOut.frame.endOfFrame
  io.videoOut.frame.line.endOfLine := pixelOut.frame.line.endOfLine
  (io.videoOut.frame.line.pixel, pixelOut.frame.line.pixel.transpose).zipped.foreach(_ := Vec(_))
}
