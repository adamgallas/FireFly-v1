package video

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class VideoPack[T <: Data](dataType: HardType[T]) extends Bundle {
  val frame = VideoFrame(dataType)
  val endOfPack = Bool()

  def arbitrationSignalFrom[T2 <: Data](that: VideoPack[T2]): Unit = {
    endOfPack := that.endOfPack
    frame.endOfFrame := that.frame.endOfFrame
    frame.line.endOfLine := that.frame.line.endOfLine
  }

  def getSignal = {
    val ret = VideoPack(NoData)
    ret.arbitrationSignalFrom(this)
    ret
  }

  def getPixel = {
    this.frame.line.pixel
  }

  def translatePixelWith[T2 <: Data](that: T2) = {
    val ret = VideoPack(that)
    ret.arbitrationSignalFrom(this)
    ret.frame.line.pixel := that
    ret
  }
}