package mlp

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class SNNMLPDataGen(
                     inputChannels: Int,
                     outputChannels: Int,
                     timeSep: Int,
                     factor: Int,
                     threshold: Int
                   ) {
  val inputData = Array.ofDim[Int](timeSep, inputChannels)
  val weightData = Array.ofDim[Int](outputChannels, inputChannels)
  val outputData = Array.ofDim[Int](timeSep, outputChannels)
  val vmemStore = Array.ofDim[Int](timeSep, outputChannels)
  val vmem = Array.ofDim[Int](outputChannels)
  val biasData = Array.ofDim[Int](outputChannels)

  def genInputStream(inParallelChannels: Int, t: Int) = {
    val length = inputChannels / inParallelChannels + (factor - (inputChannels / inParallelChannels) % factor) % factor
    inputData(t).grouped(inParallelChannels).toArray.padTo(length, Array.fill(inParallelChannels)(0))
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

  def genRandomData(winnerTakesAll: Boolean = false) = {
    for (t <- 0 until timeSep) {
      for (i <- 0 until inputChannels) {
        inputData(t)(i) = Random.nextInt(2)
      }
    }
    for (o <- 0 until outputChannels) {
      biasData(o) = Random.nextInt(65) - 32
      for (i <- 0 until inputChannels) {
        weightData(o)(i) = Random.nextInt(65) - 32
      }
    }
    for (o <- 0 until outputChannels) {
      vmem(o) = biasData(o)
    }
    for (t <- 0 until timeSep) {
      for (o <- 0 until outputChannels) {
        for (i <- 0 until inputChannels) {
          vmem(o) = vmem(o) + inputData(t)(i) * weightData(o)(i)
        }
      }

      val maxVmem = vmem.max - 1
      val compareThreshold = if (winnerTakesAll) maxVmem else threshold

      for (o <- 0 until outputChannels) {
        vmemStore(t)(o) = vmem(o)
        if (vmem(o) > compareThreshold) {
          if (!winnerTakesAll) vmem(o) = 0
          outputData(t)(o) = 1
        }
        else {
          outputData(t)(o) = 0
        }
        vmem(o) = vmem(o) + biasData(o)
      }
    }
  }
}