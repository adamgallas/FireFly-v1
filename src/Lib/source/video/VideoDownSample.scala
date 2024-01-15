package video

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object VideoDownSample {
  def apply[T <: Data](dataType: HardType[T], even: Boolean = true) = new VideoDownSample(dataType, even)
}

class VideoDownSample[T <: Data](dataType: HardType[T], even: Boolean = true) extends Component {
  val io = new Bundle {
    val videoIn = slave(Stream(VideoPack(dataType)))
    val videoOut = master(Stream(VideoPack(dataType)))
  }
  noIoPrefix()
  val pixCnt = Bool() setAsReg() init false
  val lineCnt = Bool() setAsReg() init false
  val pixCntInv = ~pixCnt
  val lineCntInv = ~lineCnt
  val fire = io.videoIn.fire

  when(fire)(pixCnt := pixCntInv)
  when(fire && io.videoIn.frame.line.endOfLine)(pixCnt.clear())
  when(fire && io.videoIn.frame.line.endOfLine)(lineCnt := lineCntInv)
  when(fire && io.videoIn.frame.endOfFrame)(lineCnt.clear())

  if (even) io.videoOut << io.videoIn.throwWhen(pixCntInv || lineCntInv)
  else io.videoOut << io.videoIn.throwWhen(pixCnt || lineCnt)
}