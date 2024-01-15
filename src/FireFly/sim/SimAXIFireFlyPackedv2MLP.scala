import nnflow.ArithmeticFactory
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba4.axilite.sim.AxiLite4Driver
import spinal.lib.tools.BigIntToListBoolean
import utils.SimMemoryMap

import scala.language.postfixOps

object SimAXIFireFlyPackedv2MLP {

  val alwaysValid = false
  val alwaysReady = false

  val inputChannels = 512 * 16
  val outputChannels = 16
  val timeStep = 4
  val threshold = 64

  val parallelChannels = 16
  val kernelSize = 3
  val width = kernelSize
  val height = kernelSize
  val factor = kernelSize * kernelSize
  val enablePooling = false
  val directWidthAdapt = true
  val winnerTakesAll = true

  val paramBusWidthReduceFactor = 2
  val paramBusWidth = parallelChannels * 8 / paramBusWidthReduceFactor
  val paramBusBytes = paramBusWidth / 8

  val totalPass = outputChannels / parallelChannels

  val SIMDIn = 2
  val paddedLength = scala.math.ceil(inputChannels.toFloat / (parallelChannels * factor).toFloat).toLong
  val numOfIFMs = paddedLength - 1
  val numOfOFMs = outputChannels / parallelChannels - 1
  val numOfTimeSteps = timeStep - 1
  val numOfTimeStepIFMs = paddedLength * timeStep - 1
  val numOfTimeStepOFMs = (outputChannels / parallelChannels) * timeStep - 1
  val weightsLength = paddedLength * parallelChannels - 1

  println(numOfIFMs, numOfTimeStepIFMs, weightsLength)

  val configReg0 = {
    (winnerTakesAll.toInt.toLong << 60) +
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

  val outputStrideOfTimeStep = outputChannels / 8
  val outputStrideOfChannel = parallelChannels / 8

  val spikeMem = new SimMemoryMap(16 MiB, 8, 16)
  val paramMem = new SimMemoryMap(16 MiB, 8, 16)

  def getFromSpikeMem = {
    val spikeMapBytes = spikeMem.mem.slice(s2mmBaseAddr.toInt, (s2mmBaseAddr + timeStep * outputChannels / 8).toInt)
    val spikeList = spikeMapBytes.flatMap(s => BigIntToListBoolean(s.toBigInt, 8 bits)).map(_.toInt)
    val spikeMap = spikeList.grouped(outputChannels).toArray
    spikeMap
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
      rtl = AXIFireFlyPackedv2(
        channels = parallelChannels,
        kernelSize = kernelSize,
        depthOfLineBuffer = 512,
        depthOfBiasFifo = 512,
        depthOfWeightFifo = 1024,
        depthOfUpdateBuffer = 4096
      )
    )
    compiled.doSimUntilVoid { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      //      SimTimeout(50000 * 10)

      val axiLiteSimThread = fork {
        val axiLiteDriver = AxiLite4Driver(dut.io.ctrl, dut.clockDomain)
        axiLiteDriver.reset()
        val pass = 16
        for (p <- 0 until pass) {

          val gen = new mlp.SNNMLPDataGen(inputChannels, outputChannels, timeStep, factor, threshold)
          gen.genRandomData(winnerTakesAll)
          val inputs = (0 until timeStep).toArray.map(i => gen.genInputStream(parallelChannels, i).flatten)
          val spikePacked = inputs.flatten.grouped(8).toArray.map(_.reverse.foldLeft(BigInt(0))((a, b) => (a << 1) + (b & 0x01))).map(_.toByte)
          Array.copy(spikePacked, 0, spikeMem.mem, mm2sBaseAddr.toInt, spikePacked.length)
          val mm2sFixLen = spikePacked.length
          val s2mmFixLen = parallelChannels / 8

          val weights = gen.genWeightStream(parallelChannels, parallelChannels, paramBusBytes).flatten.map(_.toByte)
          val bias = gen.biasData.flatMap(s => utils.BigInt2ByteArray(s.toBigInt, 2))
          Array.copy(weights, 0, paramMem.mem, 0, weights.length)
          Array.copy(bias, 0, paramMem.mem, biasTranAddr.toInt, bias.length)
          val weightTranLen = weights.length
          val biasTranLen = bias.length

          axiLiteDriver.write(0x00, configReg0)
          axiLiteDriver.write(0x08, configReg1)
          axiLiteDriver.write(0x10, configReg2)

          axiLiteDriver.write(0x20, (outputStrideOfChannel.toLong << 32) | outputStrideOfTimeStep)
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
            dut.clockDomain.waitSampling(64)
          }
          axiLiteDriver.write(0x18, 0)

          val testOutputSpike = getFromSpikeMem
          val goldOutputSpike = gen.outputData
          for (i <- 0 until timeStep) {
            for (j <- 0 until outputChannels) {
              if (testOutputSpike(i)(j) != goldOutputSpike(i)(j)) {
                println(s"t = $i, c = $j, test = ${testOutputSpike(i)(j)}, gold = ${goldOutputSpike(i)(j)}")
              }
            }
          }
          println(testOutputSpike.map(_.mkString("")).mkString(" "))
          println(goldOutputSpike.map(_.mkString("")).mkString(" "))
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
