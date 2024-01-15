package conv

import spinal.core._
import spinal.lib._

object Reducer {
  def apply[TI <: Data, TW <: Data, TM <: Data](
                                                 inDataType: HardType[TI],
                                                 weightDataType: HardType[TW],
                                                 mergeDateType: HardType[TM],

                                                 inputs: Stream[video.VideoPack[Vec[TI]]],
                                                 weights: Stream[Vec[TW]],

                                                 SIMDOut: Int,
                                                 ConvOperatorStages: Int,
                                                 ConvOperator: (TupleBundle2[Vec[TW], Vec[TI]], Vec[Bool]) => Vec[TM]
                                               ) = {

    val inputsHalted = inputs.haltWhen(!weights.valid)

    val WIData = TupleBundle2(Vec(weightDataType, weights.payload.length), Vec(inDataType, inputs.frame.line.pixel.length))
    val WIStream = inputsHalted.translateWith(inputsHalted.translatePixelWith(WIData))
    val OStream = Stream(video.VideoPack(Vec(mergeDateType, SIMDOut)))
    (WIData._1, weights.payload).zipped.foreach((x, y) => x.assignFromBits(y.asBits))
    (WIData._2, inputsHalted.frame.line.pixel).zipped.foreach((x, y) => x.assignFromBits(y.asBits))
    video.VideoMultiStageLogic(WIStream, OStream, ConvOperatorStages, ConvOperator)

    weights.ready := (inputsHalted.fire && inputsHalted.frame.endOfFrame)
    //weights.ready := OStream.fire && OStream.frame.endOfFrame

    OStream
  }
}
