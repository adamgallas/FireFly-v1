package conv

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class ConvCores[
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

               parallelChannels: Int,
               weightsFifoDepth: Int,
               lineBufferDepth: Int,
               frameBufferDepth: Int,
               kernelSize: Int,
               SIMDIn: Int,
               SIMDOut: Int,

               ConvOperatorStages: Int,
               UpdateOperatorStages: Int,
               ConvOperator: (TupleBundle2[Vec[Vec[Vec[TW]]], Vec[Vec[TI]]], Vec[Bool]) => Vec[TM],
               UpdateOperator: (TupleBundle5[Vec[TM], Vec[TU], Vec[TU], Bool, Bool], Vec[Bool]) => TupleBundle2[Vec[TU], Vec[TP]],
               largeBufferTech: String = "block",
               passThrough: Boolean = false
             ) extends Area {

  val numberOfCores = parallelChannels / SIMDOut
  val vecLength = parallelChannels * kernelSize * kernelSize

  val io = new Bundle {
    val numOfIsPerO = UInt(8 bits)
    val directWidthAdapt = Bool() default False
    val inputs = Stream(video.VideoPack(Vec(inDataType, parallelChannels)))
    val weights = Stream(Vec(weightDataType, SIMDOut * numberOfCores * SIMDIn))
    val bias = Stream(Vec(updateDataType, SIMDOut * numberOfCores))
    val outputs = Stream(video.VideoPack(Vec(postDataType, SIMDOut * numberOfCores)))
  }

  val window = InputsWidthAdapt(Vec(inDataType, parallelChannels), kernelSize, lineBufferDepth)
  val windowReshape = window.io.videoOut.translateWith(window.io.videoOut.translatePixelWith(
    window.io.videoOut.frame.line.pixel.as(Vec(Vec(inDataType, SIMDIn), vecLength / SIMDIn))
  ))
  val windowFork = StreamFork(windowReshape, numberOfCores)
  window.io.directWidthAdapt := io.directWidthAdapt
  io.inputs >> window.io.videoIn

  val weightsVec1 = io.weights.payload.as(Vec(Vec(Vec(weightDataType, SIMDOut), numberOfCores), SIMDIn))
  val weightsVec2 = Vec(Vec(Vec(weightDataType, SIMDOut), SIMDIn), numberOfCores)
  val weightsFork = (StreamFork(io.weights, numberOfCores), weightsVec2).zipped.map(_.translateWith(_))
  for (n <- 0 until numberOfCores) {
    for (i <- 0 until SIMDIn) {
      for (o <- 0 until SIMDOut) {
        weightsVec2(n)(i)(o) := weightsVec1(i)(n)(o)
      }
    }
  }

  val biasReshape = io.bias.payload.as(Vec(Vec(updateDataType, SIMDOut), numberOfCores))
  val biasFork = (StreamFork(io.bias, numberOfCores), biasReshape).zipped.map(_.translateWith(_))

  val coreOut = Vec(Stream(video.VideoPack(Vec(postDataType, SIMDOut))), numberOfCores)
  val coreOutJoin = Stream(video.VideoPack(Vec(postDataType, SIMDOut * numberOfCores)))

  val core = Array.fill(numberOfCores)(
    new ConvCore(
      inDataType, weightDataType, mergeDateType, updateDataType, postDataType,
      weightsFifoDepth, frameBufferDepth, vecLength, SIMDIn, SIMDOut,
      ConvOperatorStages, UpdateOperatorStages,
      ConvOperator, UpdateOperator,
      largeBufferTech,
      passThrough
    )
  )

  core.foreach(_.io.numOfIFMsPerOFM := io.numOfIsPerO)
  (core, windowFork).zipped.foreach(_.io.inputs << _.s2mPipe().m2sPipe())
  (core, weightsFork).zipped.foreach(_.io.weights << _)
  (core, biasFork).zipped.foreach(_.io.bias << _.s2mPipe().m2sPipe())

  (core, coreOut).zipped.foreach(_.io.outputs.s2mPipe().m2sPipe() >> _)
  coreOutJoin.arbitrationFrom(StreamJoin(coreOut))
  coreOutJoin.arbitrationSignalFrom(coreOut.head)
  coreOutJoin.frame.line.pixel.assignFromBits(Vec(coreOut.map(_.frame.line.pixel)).asBits)

  io.outputs << coreOutJoin
}
