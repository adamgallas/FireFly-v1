package video

import spinal.core._

import scala.language.postfixOps

case class VideoLine[T <: Data](dataType: HardType[T]) extends Bundle {
  val pixel = dataType()
  val endOfLine = Bool()
}