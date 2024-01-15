import nnflow.ArithmeticFactory
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.amba4.axilite.sim.AxiLite4Driver

import scala.language.postfixOps

object SimAXIFireFly {

  val alwaysValid = false
  val alwaysReady = false

  val inputChannels = 64
  val outputChannels = 64
  val timeStep = 4
  val threshold = 64
  val enablePooling = false
  val directWidthAdapt = false

  val parallelChannels = 16
  val kernelSize = 3
  val width = 16
  val height = 16

  val paramBusWidthReduceFactor = 2
  val paramBusWidth = parallelChannels * 8 / paramBusWidthReduceFactor
  val paramBusBytes = paramBusWidth / 8

  val totalPass = outputChannels / parallelChannels

  val SIMDIn = 2
  val numOfIFMs = inputChannels / parallelChannels - 1
  val numOfOFMs = outputChannels / parallelChannels - 1
  val numOfTimeSteps = timeStep - 1
  val numOfTimeStepIFMs = (inputChannels / parallelChannels) * timeStep - 1
  val numOfTimeStepOFMs = (outputChannels / parallelChannels) * timeStep - 1
  val weightsLengh = inputChannels - 1

  val configReg0 = {
    (directWidthAdapt.toInt.toLong << 56) +
      (enablePooling.toInt.toLong << 48) +
      (numOfTimeStepOFMs.toLong << 36) +
      (numOfTimeStepIFMs.toLong << 24) +
      (numOfTimeSteps.toLong << 16) +
      (numOfOFMs.toLong << 8) +
      numOfIFMs
  }

  val configReg1 =
    (weightsLengh.toLong << 32) +
      ((height - 1).toLong << 16) +
      width - 1

  val gen = new conv.SNNConvDataGen(inputChannels, outputChannels, timeStep, threshold, kernelSize, width, height)
  gen.genRandomData()
  val weights = gen.genWeightStream(parallelChannels, parallelChannels, paramBusBytes)
  val bias = gen.genBiasStream(paramBusBytes / 2)

  def driverInputs(drain: Bits, source: Array[Int]): Unit = {
    drain #= source.reverse.foldLeft(0)((i, b) => (i << 1) + (if (b == 0) 0 else 1))
  }

  def driverParamsInShorts(drain: Bits, source: Array[Int]): Unit = {
    drain #= utils.ShortArray2BigInt(source.map(_.toShort))
  }

  def driverParamsInBytes(drain: Bits, source: Array[Int]): Unit = {
    drain #= utils.ByteArray2BigInt(source.map(_.toByte))
  }

  ArithmeticFactory.setUseModel()
  def main(args: Array[String]): Unit = {
    val compiled = SimConfig.withWave.allOptimisation.compile(
      rtl = AXIFireFly(
        channels = parallelChannels,
        kernelSize = kernelSize,
        depthOfLineBuffer = 512,
        depthOfBiasFifo = 512,
        depthOfWeightFifo = 512,
        depthOfUpdateBuffer = 4096
      )
    )
    compiled.doSimUntilVoid { dut =>
      dut.io.inputs.valid #= false
      dut.io.params.valid #= false
      dut.clockDomain.forkStimulus(period = 10)
      SimTimeout(100000 * 10)

      val axiLiteDriver = AxiLite4Driver(dut.io.ctrl, dut.clockDomain)
      axiLiteDriver.reset()
      axiLiteDriver.write(0x00, configReg0)
      axiLiteDriver.write(0x08, configReg1)
      axiLiteDriver.write(0x10, threshold)

      fork {
        dut.io.inputs.valid #= false
        dut.clockDomain.waitSampling(128)
        for (iter <- 0 until totalPass) {
          for (time <- 0 until timeStep) {
            driverInputs(dut.io.inputs.payload, Array.fill(parallelChannels)(0))
            dut.io.inputs.valid #= false
            dut.clockDomain.waitSampling(8)
            utils.SimArray2Stream(dut.io.inputs, gen.genInputStream(parallelChannels, time), dut.clockDomain, driverInputs, alwaysValid = alwaysValid)
            dut.io.inputs.valid #= false
            driverInputs(dut.io.inputs.payload, Array.fill(parallelChannels)(0))
            dut.clockDomain.waitSampling(8)
          }
        }
        dut.clockDomain.waitSampling(256)
      }

      fork {
        // bias
        dut.io.params.valid #= false
        dut.clockDomain.waitSampling(128)
        utils.SimArray2StreamFragment(dut.io.params, bias, dut.clockDomain, driverParamsInShorts, alwaysValid = alwaysValid)
        dut.io.params.valid #= false
        dut.clockDomain.waitSampling(32)

        utils.SimArray2StreamFragment(dut.io.params, weights, dut.clockDomain, driverParamsInBytes, alwaysValid = alwaysValid)
        dut.io.params.valid #= false
      }

      fork {
        var totErrCnt = 0
        if (!enablePooling) {
          for (iter <- 0 until totalPass) {
            for (time <- 0 until timeStep) {
              var errCnt = 0
              println(iter, time)
              dut.io.outputs.ready #= false
              var index = 0
              while (index < width * height) {
                val (r, c) = (index / width, index % width)
                if (alwaysReady)
                  dut.io.outputs.ready #= true
                else
                  dut.io.outputs.ready.randomize()
                dut.clockDomain.waitSampling()
                if (dut.io.outputs.valid.toBoolean && dut.io.outputs.ready.toBoolean) {
                  val testBatch = spinal.lib.tools.BigIntToListBoolean(dut.io.outputs.payload.toBigInt, parallelChannels bits)
                  for (ch <- 0 until parallelChannels) {
                    val test = testBatch(ch).toInt
                    val gold = gen.outputData(time)(iter * parallelChannels + ch)(r)(c)
                    val goldVmem = gen.vmemStore(time)(iter * parallelChannels + ch)(r)(c)
                    if (test != gold) {
                      println(time, iter * parallelChannels + ch, index, test, "glod=", gold, "vmem=", goldVmem)
                      errCnt = errCnt + 1
                    }
                  }
                  index = index + 1
                }
              }
              totErrCnt = totErrCnt + errCnt
            }
          }
          println(totErrCnt.toFloat / (width * height * timeStep * outputChannels))
        }
        else {
          val halfWidth = width / 2
          val halfHeight = height / 2
          val poolOutputs = gen.outputMaxPooling2x2()
          for (iter <- 0 until totalPass) {
            for (time <- 0 until timeStep) {
              var errCnt = 0
              println(iter, time)
              if (alwaysReady)
                dut.io.outputs.ready #= true
              else
                dut.io.outputs.ready.randomize()
              var index = 0
              while (index < halfWidth * halfHeight) {
                val (r, c) = (index / halfWidth, index % halfWidth)
                dut.io.outputs.ready #= true
                dut.clockDomain.waitSampling()
                if (dut.io.outputs.valid.toBoolean && dut.io.outputs.ready.toBoolean) {
                  val testBatch = spinal.lib.tools.BigIntToListBoolean(dut.io.outputs.payload.toBigInt, parallelChannels bits)
                  for (ch <- 0 until parallelChannels) {
                    val test = testBatch(ch).toInt
                    val gold = poolOutputs(time)(iter * parallelChannels + ch)(r)(c)
                    if (test != gold) {
                      println(time, iter * parallelChannels + ch, index, test, gold)
                      errCnt = errCnt + 1
                    }
                  }
                  index = index + 1
                }
              }
              totErrCnt = totErrCnt + errCnt
            }
          }
          println(totErrCnt.toFloat / (halfWidth * halfHeight * timeStep * outputChannels))
        }
        dut.clockDomain.waitSampling(64)
        println("apdone", axiLiteDriver.read(0x18).toInt)
        axiLiteDriver.write(0x18, 0)
        println("apdone", axiLiteDriver.read(0x18).toInt)
        simSuccess()
      }
    }
  }
}
