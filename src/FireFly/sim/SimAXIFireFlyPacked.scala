import nnflow.ArithmeticFactory
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba4.axilite.sim.AxiLite4Driver
import spinal.lib.tools.BigIntToListBoolean
import utils.SimMemoryMap

import scala.language.postfixOps

object SimAXIFireFlyPacked {

  val alwaysValid = false
  val alwaysReady = false

  val inputChannels = 64
  val outputChannels = 128
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
  val weightsLength = inputChannels - 1

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
    (weightsLength.toLong << 32) +
      ((height - 1).toLong << 16) +
      width - 1

  val configReg2 = threshold

  val strideOfChannel = 8 KiB
  val strideOfTimeStep = 512 KiB
  val mm2sBaseAddr = 0
  val s2mmBaseAddr = 8 MiB
  val weightTranAddr = 0
  val biasTranAddr = 8 MiB
  val mm2sFixLen = width * height * parallelChannels / 8
  val s2mmFixLen = if (enablePooling) mm2sFixLen / 4 else mm2sFixLen
  val weightTranLen = inputChannels * outputChannels * kernelSize * kernelSize
  val biasTranLen = outputChannels * 2

  val gen = new conv.SNNConvDataGen(inputChannels, outputChannels, timeStep, threshold, kernelSize, width, height)
  gen.genRandomData()
  val weights = gen.genWeightStream(parallelChannels, parallelChannels, paramBusBytes).flatten.map(_.toByte)
  val bias = gen.biasData.flatMap(s => utils.BigInt2ByteArray(s.toBigInt, 2))

  val spikeMem = new SimMemoryMap(16 MiB, 8, 16)
  val paramMem = new SimMemoryMap(16 MiB, 8, 16)
  Array.copy(weights, 0, paramMem.mem, 0, weights.length)
  Array.copy(bias, 0, paramMem.mem, biasTranAddr.toInt, bias.length)

  val inputStrideOfTimeStep = strideOfTimeStep
  val inputStrideOfChannel = strideOfChannel

  val outputStrideOfTimeStep = strideOfTimeStep
  val outputStrideOfChannel = strideOfChannel

  for (t <- 0 until timeStep) {
    for (c <- 0 until inputChannels / parallelChannels) {
      val spikeMap = (for (p <- 0 until parallelChannels) yield
        gen.inputData(t)(c * parallelChannels + p).flatten).transpose
      val spikeMapPacked = spikeMap.map(_.reverse.foldLeft(BigInt(0))((a, b) => (a << 1) + (b & 0x01)))
      val spikeMapBytes = spikeMapPacked.flatMap(s => utils.BigInt2ByteArray(s, parallelChannels / 8))
      val addr = mm2sBaseAddr + t * inputStrideOfTimeStep + c * inputStrideOfChannel
      Array.copy(spikeMapBytes.toArray, 0, spikeMem.mem, addr.toInt, spikeMapBytes.length)
    }
  }

  def getFromSpikeMem = {
    val outWidth = if (enablePooling) width / 2 else width
    (for (t <- 0 until timeStep) yield {
      (for (c <- 0 until outputChannels / parallelChannels) yield {
        val addr = s2mmBaseAddr + t * outputStrideOfTimeStep + c * outputStrideOfChannel
        val spikeMapBytes = spikeMem.mem.slice(addr.toInt, (addr + s2mmFixLen).toInt)
        val spikeList = spikeMapBytes.flatMap(s => BigIntToListBoolean(s.toBigInt, 8 bits)).map(_.toInt)
        val spikeMap = spikeList.grouped(parallelChannels).toArray.transpose.map(_.grouped(outWidth).toArray)
        spikeMap
      }).toArray
    }).toArray
  }

  def inputsDriver(dest: Bits, source: BigInt) = {
    dest #= source
  }

  def outputsDriver(source: Bits) = {
    source.toBigInt
  }

  ArithmeticFactory.setUseModel()

  def main(args: Array[String]): Unit = {
    val compiled = SimConfig.allOptimisation.compile(
      rtl = AXIFireFlyPacked(
        channels = parallelChannels,
        kernelSize = kernelSize,
        depthOfLineBuffer = 512,
        depthOfBiasFifo = 512,
        depthOfWeightFifo = 512,
        depthOfUpdateBuffer = 4096
      )
    )
    compiled.doSimUntilVoid { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      //SimTimeout(100000 * 10)

      val axiLiteSimThread = fork {
        val axiLiteDriver = AxiLite4Driver(dut.io.ctrl, dut.clockDomain)
        axiLiteDriver.reset()
        val pass = 16
        for (p <- 0 until pass) {

          axiLiteDriver.write(0x00, configReg0)
          axiLiteDriver.write(0x08, configReg1)
          axiLiteDriver.write(0x10, configReg2)

          axiLiteDriver.write(0x20, (strideOfChannel.toLong << 32) | strideOfTimeStep)
          axiLiteDriver.write(0x28, (s2mmBaseAddr.toLong << 32) | mm2sBaseAddr)
          axiLiteDriver.write(0x30, (s2mmFixLen.toLong << 32) | mm2sFixLen)
          axiLiteDriver.write(0x38, (weightTranLen.toLong << 32) | weightTranAddr)
          axiLiteDriver.write(0x40, (biasTranLen.toLong << 32) | biasTranAddr)
          axiLiteDriver.write(0x48, 0x100000000L)
          dut.clockDomain.waitSampling(8)
          axiLiteDriver.write(0x48, 0x100000000L)
          dut.clockDomain.waitSampling(8)
          axiLiteDriver.write(0x48, 0x000010001L)

          while (axiLiteDriver.read(0x18) == 0) {
            dut.clockDomain.waitSampling()
          }
          axiLiteDriver.write(0x18, 0)

          val testOutputSpike = getFromSpikeMem
          val goldOutputSpike = if (enablePooling) gen.outputMaxPooling2x2() else gen.outputData
          val outWidth = if (enablePooling) width / 2 else width
          val outHeight = if (enablePooling) height / 2 else height
          for (t <- 0 until timeStep) {
            for (c <- 0 until outputChannels / parallelChannels) {
              for (p <- 0 until parallelChannels) {
                for (h <- 0 until outHeight) {
                  for (w <- 0 until outWidth) {
                    val test = testOutputSpike(t)(c)(p)(h)(w)
                    val gold = goldOutputSpike(t)(c * parallelChannels + p)(h)(w)
                    if (test != gold) {
                      println(s"t = $t, c = $c, p = $p, h = $h, w = $w, test = $test, gold = $gold")
                    }
                  }
                }
              }
            }
          }
          println(s"pass $p finished!")
        }

        simSuccess()
      }

      spikeMem.mm2sCmdSimThread(dut.io.mm2sCmd, dut.clockDomain)
      spikeMem.mm2sPendingSimThread(dut.clockDomain)
      spikeMem.s2mmCmdSimThread(dut.io.s2mmCmd, dut.clockDomain)
      spikeMem.s2mmPendingSimThread(dut.clockDomain)
      spikeMem.mm2sDataSimThread(dut.io.inputs, dut.clockDomain, inputsDriver)
      spikeMem.s2mmDataSimThread(dut.io.outputs, dut.clockDomain, outputsDriver)

      paramMem.mm2sCmdSimThread(dut.io.paramCmd, dut.clockDomain)
      paramMem.mm2sPendingSimThread(dut.clockDomain)
      paramMem.mm2sFragmentDataSimThread(dut.io.params, dut.clockDomain, inputsDriver)
    }
  }
}
