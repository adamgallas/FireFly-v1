package video

import spinal.core._

import scala.language.postfixOps

case class VideoFrame[T <: Data](dataType: HardType[T]) extends Bundle {
  val line = VideoLine(dataType)
  val endOfFrame = Bool()
}