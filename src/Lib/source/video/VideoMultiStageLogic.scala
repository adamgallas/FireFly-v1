package video

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object VideoMultiStageLogic {
  def apply[T1 <: Data, T2 <: Data](
                                     videoIn: Stream[VideoPack[T1]],
                                     videoOut: Stream[VideoPack[T2]],
                                     stage: Int,
                                     op: (T1, Vec[Bool]) => T2
                                   ) = {
    require(stage >= 1)

    def videoPipes(packIn: VideoPack[NoData], cond: Vec[Bool]): VideoPack[NoData] = {
      val pack = Vec(VideoPack(NoData), cond.length)
      pack.head := packIn
      for (i <- 0 until cond.length - 1) pack(i + 1) := RegNextWhen(pack(i), cond(i))
      RegNextWhen(pack.last, cond.last)
    }

    val eventOutPipes = utils.EventM2sPipes(videoIn.toEvent(), stage)
    videoOut.frame.line.pixel := op(videoIn.frame.line.pixel, eventOutPipes._2)
    videoOut.arbitrationFrom(eventOutPipes._1)
    videoOut.payload.arbitrationSignalFrom(videoPipes(videoIn.getSignal, eventOutPipes._2))
  }
}

class VideoMultiStageLogic[T1 <: Data, T2 <: Data](
                                                    inDataType: HardType[T1],
                                                    outDataType: HardType[T2],
                                                    stage: Int,
                                                    op: (T1, Vec[Bool]) => T2
                                                  ) extends Component {
  val io = new Bundle {
    val videoIn = slave(Stream(VideoPack(inDataType)))
    val videoOut = master(Stream(VideoPack(outDataType)))
  }
  noIoPrefix()
  require(stage >= 1)
  VideoMultiStageLogic(io.videoIn, io.videoOut, stage, op)
}
