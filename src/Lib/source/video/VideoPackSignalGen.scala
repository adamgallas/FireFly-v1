package video

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object VideoPackSignalGen {
  def apply[T <: Data](in: Stream[T], maxLine: UInt, maxFrame: UInt, maxPack: UInt): Stream[VideoPack[T]] = {
    val fire = in.fire
    val signal = VideoPack(NoData)
    val videoPack = signal.translatePixelWith(in.payload)
    val lineCnt = Counter(maxLine.getWidth bits)
    val frameCnt = Counter(maxFrame.getWidth bits)
    val packCnt = Counter(maxPack.getWidth bits)

    val lineOverflow = lineCnt === maxLine
    val frameOverflow = frameCnt === maxFrame
    val packOverflow = packCnt === maxPack

    signal.frame.line.endOfLine := lineOverflow
    signal.frame.endOfFrame := lineOverflow && frameOverflow
    signal.endOfPack := lineOverflow && frameOverflow && packOverflow

    when(in.fire) {
      lineCnt.increment()
      when(lineOverflow) {
        lineCnt.clear()
        frameCnt.increment()
        when(frameOverflow) {
          frameCnt.clear()
          packCnt.increment()
          when(packOverflow) {
            packCnt.clear()
          }
        }
      }
    }

    in.translateWith(videoPack)
  }

}
