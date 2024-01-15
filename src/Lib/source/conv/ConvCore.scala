package conv

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class ConvCore[
  TI <: Data,
  TW <: Data,
  TM <: Data,
  TU <: Data,
  TP <: Data](
               inDataType: HardType[TI],
               weightDataType: HardType[TW],
               mergeDateType: HardType[TM],
               updateDataType: HardType[TU],
               postDataType: HardType[TP],

               weightsFifoDepth: Int,
               frameBufferDepth: Int,

               VecLength: Int,
               SIMDIn: Int,
               SIMDOut: Int,

               ConvOperatorStages: Int,
               UpdateOperatorStages: Int,
               ConvOperator: (TupleBundle2[Vec[Vec[Vec[TW]]], Vec[Vec[TI]]], Vec[Bool]) => Vec[TM],
               UpdateOperator: (TupleBundle5[Vec[TM], Vec[TU], Vec[TU], Bool, Bool], Vec[Bool]) => TupleBundle2[Vec[TU], Vec[TP]],
               largeBufferTech: String = "block",
               passThrough: Boolean = false
             ) extends Area {

  val length = VecLength / SIMDIn
  val io = new Bundle {
    val numOfIFMsPerOFM = UInt(8 bits)
    val inputs = Stream(video.VideoPack(Vec(Vec(inDataType, SIMDIn), length)))
    val weights = Stream(Vec(Vec(weightDataType, SIMDOut), SIMDIn))
    val bias = Stream(Vec(updateDataType, SIMDOut))
    val outputs = Stream(video.VideoPack(Vec(postDataType, SIMDOut)))
  }

  val weightsDistr = new WeightsDistributor(weightDataType, SIMDIn, SIMDOut, length, weightsFifoDepth)
  weightsDistr.io.inputs << io.weights

  val mergedOutputs = Reducer(
    Vec(inDataType, SIMDIn), Vec(Vec(weightDataType, SIMDOut), SIMDIn), mergeDateType,
    io.inputs, weightsDistr.io.outputs, SIMDOut, ConvOperatorStages, ConvOperator
  )

  video.VideoFrameUpdate(
    io.numOfIFMsPerOFM, io.bias, mergedOutputs.s2mPipe().m2sPipe(), io.outputs,
    frameBufferDepth, UpdateOperatorStages, UpdateOperator, largeBufferTech, passThrough
  )
}
