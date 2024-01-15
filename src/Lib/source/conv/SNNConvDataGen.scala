package conv

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import scala.util.Random

class SNNConvDataGen(
                      inputChannels: Int,
                      outputChannels: Int,
                      timeStep: Int,
                      threshold: Int,
                      kernelSize: Int,
                      width: Int,
                      height: Int
                    ) {

  val inputData = Array.ofDim[Int](timeStep, inputChannels, height, width)
  val weightData = Array.ofDim[Int](outputChannels, inputChannels, kernelSize, kernelSize)
  val outputData = Array.ofDim[Int](timeStep, outputChannels, height, width)
  val biasData = Array.ofDim[Int](outputChannels)
  val vmemStore = Array.ofDim[Int](timeStep, outputChannels, width, height)

  val heightRange = 0 until height
  val widthRange = 0 until width
  val kernelSizeHalf = (kernelSize - 1) / 2
  val kernelRange = -kernelSizeHalf to kernelSizeHalf

  def genInputStream(
                      inParallelChannels: Int,
                      timeIndex: Int
                    ) = {
    val ret = ArrayBuffer[Array[Int]]()
    val numberOfFrames = inputChannels / inParallelChannels
    for (n <- 0 until numberOfFrames) {
      for (r <- heightRange) {
        for (c <- widthRange) {
          val data = ArrayBuffer[Int]()
          for (s <- 0 until inParallelChannels) {
            data.append(inputData(timeIndex)(n * inParallelChannels + s)(r)(c))
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

  def genWeightStreamInTime(
                             inParallelChannels: Int,
                             outParallelChannels: Int,
                             busParallelChannels: Int
                           ) = {
    val flatten = ArrayBuffer[Int]()
    for (och <- 0 until outputChannels / outParallelChannels) {
      for (t <- 0 until timeStep) {
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
    }
    flatten.toArray.grouped(busParallelChannels).toArray
  }

  def outputMaxPooling2x2(): Array[Array[Array[Array[Int]]]] = {
    require(width % 2 == 0 && height % 2 == 0)
    val ret = Array.ofDim[Int](timeStep, outputChannels, height / 2, width / 2)
    for (t <- 0 until timeStep) {
      for (o <- 0 until outputChannels) {
        for (r <- 0 until height / 2) {
          for (c <- 0 until width / 2) {
            val pix = Array(
              outputData(t)(o)(r * 2)(c * 2),
              outputData(t)(o)(r * 2 + 1)(c * 2),
              outputData(t)(o)(r * 2)(c * 2 + 1),
              outputData(t)(o)(r * 2 + 1)(c * 2 + 1)
            )
            ret(t)(o)(r)(c) = pix.max
          }
        }
      }
    }
    ret
  }

  def genRandomData(): Unit = {
    for (t <- 0 until timeStep) {
      for (i <- 0 until inputChannels) {
        for (r <- heightRange) {
          for (c <- widthRange) {
            inputData(t)(i)(r)(c) = Random.nextInt(2)
            //inputData(t)(i)(r)(c) = if (i < 8) 1 else 0
          }
        }
      }
    }
    for (o <- 0 until outputChannels) {
      for (i <- 0 until inputChannels) {
        for (kR <- 0 until kernelSize) {
          for (kC <- 0 until kernelSize) {
            weightData(o)(i)(kR)(kC) = Random.nextInt(64) - 32
            //weightData(o)(i)(kR)(kC) = o
          }
        }
      }
    }
    for (o <- 0 until outputChannels) {
//            biasData(o) = Random.nextInt(128)
      biasData(o) = Random.nextInt(256) - 128
      //      biasData(o) = 0
    }

    val vmem = Array.ofDim[Int](outputChannels, width, height)

    for (o <- 0 until outputChannels) {
      for (r <- heightRange) {
        for (c <- widthRange) {
          vmem(o)(r)(c) = biasData(o)
        }
      }
    }

    for (t <- 0 until timeStep) {
      for (o <- 0 until outputChannels) {
        for (r <- heightRange) {
          for (c <- widthRange) {
            for (i <- 0 until inputChannels) {
              for (kR <- kernelRange) {
                for (kC <- kernelRange) {
                  val (ros, cos) = (r + kR, c + kC)
                  if (heightRange.contains(ros) && widthRange.contains(cos)) {
                    if (inputData(t)(i)(ros)(cos) == 1)
                      vmem(o)(r)(c) += weightData(o)(i)(kR + kernelSizeHalf)(kC + kernelSizeHalf)
                  }
                }
              }
            }
          }
        }
      }

      for (o <- 0 until outputChannels) {
        for (r <- heightRange) {
          for (c <- widthRange) {
            vmemStore(t)(o)(r)(c) = vmem(o)(r)(c)
            if (vmem(o)(r)(c) > threshold) {
              vmem(o)(r)(c) = 0
              outputData(t)(o)(r)(c) = 1
            }
            else {
              outputData(t)(o)(r)(c) = 0
            }
            vmem(o)(r)(c) = vmem(o)(r)(c) + biasData(o)
          }
        }
      }
    }
  }
}
