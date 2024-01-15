package conv

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class ConvDataGen(
                   inputChannels: Int,
                   outputChannels: Int,
                   kernelSize: Int,
                   width: Int,
                   height: Int,
                   scaleBase: Int = 65536,
                   scaleExpr: Int = 0,
                   inZeroPoint: Int = 0,
                   outZeroPoint: Int = 0
                 ) {

  val inputData = Array.ofDim[Int](inputChannels, height, width)
  val weightData = Array.ofDim[Int](outputChannels, inputChannels, kernelSize, kernelSize)
  val outputData = Array.ofDim[Int](outputChannels, height, width)
  val biasData = Array.ofDim[Int](outputChannels)

  val heightRange = 0 until height
  val widthRange = 0 until width
  val kernelSizeHalf = (kernelSize - 1) / 2
  val kernelRange = -kernelSizeHalf to kernelSizeHalf

  def genInputVideoPack(inParallelChannels: Int): Array[(Array[Int], Boolean, Boolean, Boolean)] = {
    val ret = ArrayBuffer[(Array[Int], Boolean, Boolean, Boolean)]()
    val numberOfFrames = inputChannels / inParallelChannels
    for (t <- 0 until numberOfFrames) {
      for (r <- heightRange) {
        for (c <- widthRange) {
          val data = ArrayBuffer[Int]()
          for (s <- 0 until inParallelChannels) {
            data.append(inputData(t * inParallelChannels + s)(r)(c))
          }
          ret.append((
            data.toArray,
            c == width - 1,
            r == height - 1 && c == width - 1,
            r == height - 1 && c == width - 1 && t == numberOfFrames - 1
          ))
        }
      }
    }
    ret.toArray
  }

  def genInputStream(
                      inParallelChannels: Int
                    ) = {
    val ret = ArrayBuffer[Array[Int]]()
    val numberOfFrames = inputChannels / inParallelChannels
    for (t <- 0 until numberOfFrames) {
      for (r <- heightRange) {
        for (c <- widthRange) {
          val data = ArrayBuffer[Int]()
          for (s <- 0 until inParallelChannels) {
            data.append(inputData(t * inParallelChannels + s)(r)(c))
          }
          ret.append(data.toArray)
        }
      }
    }
    ret.toArray
  }

  def genBiasStream(
                     outParallelChannels: Int
                   ) = {
    biasData.grouped(outParallelChannels).toArray
  }

  def genWeightStream(
                       inParallelChannels: Int,
                       outParallelChannels: Int,
                       busParallelChannels: Int
                     ) = {
    val flatten = ArrayBuffer[Int]()
    for (och <- 0 until outputChannels / outParallelChannels) {
      for (ich <- 0 until inputChannels / inParallelChannels) {
        for (kR <- 0 until kernelSize) {
          for (kC <- 0 until kernelSize) {
            for (i <- 0 until inParallelChannels) {
              for (o <- 0 until outParallelChannels) {
                flatten.append(weightData(och * outParallelChannels + o)(ich * inParallelChannels + i)(kR)(kC))
              }
            }
          }
        }
      }
    }
    flatten.toArray.grouped(busParallelChannels).toArray
  }

  def outputMaxPooling2x2(): Array[Array[Array[Int]]] = {
    require(width % 2 == 0 && height % 2 == 0)
    val ret = Array.ofDim[Int](outputChannels, height / 2, width / 2)
    for (o <- 0 until outputChannels) {
      for (r <- 0 until height / 2) {
        for (c <- 0 until width / 2) {
          val pix = Array(
            outputData(o)(r * 2)(c * 2),
            outputData(o)(r * 2 + 1)(c * 2),
            outputData(o)(r * 2)(c * 2 + 1),
            outputData(o)(r * 2 + 1)(c * 2 + 1)
          )
          ret(o)(r)(c) = pix.max
        }
      }
    }
    ret
  }

  def genRandomData(): Unit = {
    for (i <- 0 until inputChannels) {
      for (r <- heightRange) {
        for (c <- widthRange) {
          inputData(i)(r)(c) = Random.nextInt(128) - 64 + inZeroPoint
          //inputData(i)(r)(c) = (i + r + c) % 64 - 32
        }
      }
    }
    for (o <- 0 until outputChannels) {
      for (i <- 0 until inputChannels) {
        for (kR <- 0 until kernelSize) {
          for (kC <- 0 until kernelSize) {
            weightData(o)(i)(kR)(kC) = Random.nextInt(32) - 16
            //weightData(o)(i)(kR)(kC) = (o + i + kR + kC) % 64 - 32
          }
        }
      }
    }
    for (o <- 0 until outputChannels) {
      biasData(o) = Random.nextInt(512) - 256
      //biasData(o) = 0
    }
    for (o <- 0 until outputChannels) {
      for (r <- heightRange) {
        for (c <- widthRange) {
          outputData(o)(r)(c) = biasData(o)
          for (i <- 0 until inputChannels) {
            for (kR <- kernelRange) {
              for (kC <- kernelRange) {
                val (ros, cos) = (r + kR, c + kC)
                if (heightRange.contains(ros) && widthRange.contains(cos)) {
                  outputData(o)(r)(c) +=
                    (inputData(i)(ros)(cos) - inZeroPoint) * weightData(o)(i)(kR + kernelSizeHalf)(kC + kernelSizeHalf)
                }
              }
            }
          }
          outputData(o)(r)(c) = math.round(outputData(o)(r)(c).toFloat * scaleBase / math.pow(2, scaleExpr + 16)).toInt + outZeroPoint
        }
      }
    }
  }
}
