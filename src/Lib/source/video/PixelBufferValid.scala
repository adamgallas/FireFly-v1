package video

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object PixelBufferValid {
  def apply[T <: Data](dataType: HardType[T], depth: Int) = new PixelBufferValid(dataType, depth)
}

class PixelBufferValid[T <: Data](dataType: HardType[T], depth: Int) extends Component {
  require(depth > 1)
  val io = new Bundle {
    val videoIn = slave(Stream(VideoPack(dataType)))
    val videoOut = master(Stream(VideoPack(Vec(dataType, depth))))
  }
  noIoPrefix()

  val videoInFire = io.videoIn.fire
  val videoOutFire = io.videoOut.fire
  val pixelBuffer = (Bits(widthOf(dataType) * (depth - 1) bits)).setAsReg()
  when(videoInFire) {
    pixelBuffer := io.videoIn.frame.line.pixel ## (pixelBuffer >> widthOf(dataType))
  }
  val bridge = Stream(VideoPack(Vec(dataType, depth)))
  val bridgeFire = bridge.fire
  bridge.arbitrationFrom(io.videoIn)

  val pixelCnt = Counter(depth)
  val pixelCntWontOverflowIfInc = !pixelCnt.willOverflowIfInc
  when(videoInFire && io.videoIn.frame.line.endOfLine)(pixelCnt.clear())
  when(videoInFire && pixelCntWontOverflowIfInc)(pixelCnt.increment())

  val pixelVec = Vec(dataType, depth)
  pixelVec.assignFromBits(io.videoIn.frame.line.pixel ## pixelBuffer)
  bridge.endOfPack := io.videoIn.endOfPack
  bridge.frame.endOfFrame := io.videoIn.frame.endOfFrame
  bridge.frame.line.endOfLine := io.videoIn.frame.line.endOfLine
  bridge.frame.line.pixel := pixelVec

  io.videoOut << bridge.throwWhen(pixelCntWontOverflowIfInc)
}
