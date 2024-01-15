package mlp

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class MLPDataGen(
                  inputChannels: Int,
                  outputChannels: Int,
                  factor: Int,
                  scaleBase: Int = 65536,
                  scaleExpr: Int = 0,
                  inZeroPoint: Int = 0,
                  outZeroPoint: Int = 0
                ) {
  val inputData = Array.ofDim[Int](inputChannels)
  val weightData = Array.ofDim[Int](outputChannels, inputChannels)
  val outputData = Array.ofDim[Int](outputChannels)
  val outputDataAfterScale = Array.ofDim[Int](outputChannels)
  val biasData = Array.ofDim[Int](outputChannels)

  def genInputStream(inParallelChannels: Int) = {
    val length = inputChannels / inParallelChannels + (factor - (inputChannels / inParallelChannels) % factor) % factor
    inputData.grouped(inParallelChannels).toArray.padTo(length, Array.fill(inParallelChannels)(inZeroPoint))
  }

  def genWeightStream(
                       inParallelChannels: Int,
                       outParallelChannels: Int,
                       busParallelChannels: Int
                     ) = {
    val length = inputChannels / inParallelChannels + (factor - (inputChannels / inParallelChannels) % factor) % factor
    val flatten = ArrayBuffer[Int]()
    for (och <- 0 until outputChannels / outParallelChannels) {
      for (ich <- 0 until length) {
        for (i <- 0 until inParallelChannels) {
          for (o <- 0 until outParallelChannels) {
            if (ich < inputChannels / inParallelChannels)
              flatten.append(weightData(och * outParallelChannels + o)(ich * inParallelChannels + i))
            else
              flatten.append(0)
          }
        }
      }
    }
    flatten.toArray.grouped(busParallelChannels).toArray
  }

  def genBiasStream(
                     outParallelChannels: Int
                   ) = {
    biasData.grouped(outParallelChannels).toArray
  }

  def genRandomData() = {
    for (i <- 0 until inputChannels) {
      inputData(i) = Random.nextInt(64) - 32 + inZeroPoint
    }
    for (o <- 0 until outputChannels) {
      biasData(o) = Random.nextInt(64) - 32
      for (i <- 0 until inputChannels) {
        weightData(o)(i) = Random.nextInt(64) - 32
      }
    }
    for (o <- 0 until outputChannels) {
      outputData(o) = biasData(o)
      for (i <- 0 until inputChannels) {
        outputData(o) = outputData(o) + (inputData(i) - inZeroPoint) * weightData(o)(i)
      }
      outputDataAfterScale(o) = math.round(outputData(o).toFloat * scaleBase / math.pow(2, scaleExpr + 16)).toInt + outZeroPoint
    }
  }
}