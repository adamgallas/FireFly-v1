import nnflow.ArithmeticFactory
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba4.axilite.sim.AxiLite4Driver
import spinal.lib.tools.BigIntToListBoolean
import utils.SimMemoryMap

import scala.language.postfixOps

object SimAXIFireFlyPackedv3 {

  val alwaysValid = false
  val alwaysReady = false

  val inputChannels = 32
  val outputChannels = 64
  val timeStep = 4
  val threshold = 64
  val enablePooling = true
  val directWidthAdapt = false
  val winnerTakesAll = false
  val parallelChannels = 16
  val kernelSize = 3
  val width = 8
  val height = 8

  val outWidth = if (enablePooling) width / 2 else width
  val outHeight = if (enablePooling) height / 2 else height

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

  val strideOfChannel = 8 KiB
  val strideOfTimeStep = 512 KiB
  val mm2sBaseAddr = 0
  val s2mmBaseAddr = 8 MiB
  val weightTranAddr = 0
  val biasTranAddr = 8 MiB
  val mm2sFixLen = timeStep * width * height * inputChannels / 8
  val s2mmFixLen = outWidth * outHeight * parallelChannels / 8
  val weightTranLen = inputChannels * outputChannels * kernelSize * kernelSize
  val biasTranLen = outputChannels * 2

  val inputStrideOfTimeStep = width * height * inputChannels / 8
  val inputStrideOfChannel = width * height * parallelChannels / 8

  val outputStrideOfTimeStep = outWidth * outHeight * outputChannels / 8
  val outputStrideOfChannel = outWidth * outHeight * parallelChannels / 8

  val spikeMem = new SimMemoryMap(16 MiB, 8, 16, mm2sDataValidRand = false, s2mmDataReadyRand = false)
  val paramMem = new SimMemoryMap(16 MiB, 8, 16, mm2sDataValidRand = false, s2mmDataReadyRand = false)

  val configReg_0x00 = {
    (numOfTimeSteps.toLong << 16) +
      (numOfOFMs.toLong << 8) +
      numOfIFMs
  }

  val configReg_0x04 = {
    (numOfTimeStepOFMs.toLong << 12) +
      numOfTimeStepIFMs.toLong
  }

  val configReg_0x08 = {
    (threshold.toLong << 14) +
      (weightsLength)
  }

  val configReg_0x0c = {
    (winnerTakesAll.toInt.toLong << 30) +
      (directWidthAdapt.toInt.toLong << 29) +
      (enablePooling.toInt.toLong << 28) +
      ((height - 1).toLong << 16) +
      width - 1
  }

  val configReg_0x20 = outputStrideOfTimeStep
  val configReg_0x24 = outputStrideOfChannel
  val configReg_0x28 = mm2sBaseAddr
  val configReg_0x2c = s2mmBaseAddr.toInt
  val configReg_0x30 = mm2sFixLen
  val configReg_0x34 = s2mmFixLen
  val configReg_0x38 = weightTranAddr
  val configReg_0x3c = weightTranLen
  val configReg_0x40 = biasTranAddr.toInt
  val configReg_0x44 = biasTranLen

  def getFromSpikeMem = {
    // (t c h w) p
    // p (t c h) w
    // p w (t c h)
    // p w h c t
    val spikeMapBytes = spikeMem.mem.slice(s2mmBaseAddr.toInt, (s2mmBaseAddr + timeStep * outHeight * outWidth * outputChannels / 8).toInt)
    val spikeList = spikeMapBytes.flatMap(s => BigIntToListBoolean(s.toBigInt, 8 bits)).map(_.toInt)
    val spikeMap = spikeList.grouped(parallelChannels).toArray.transpose.map(
      _.grouped(outWidth).toArray.transpose.map(
        _.grouped(outHeight).toArray.transpose.map(
          _.grouped(outputChannels / parallelChannels).toArray.transpose)))
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
      rtl = AXIFireFlyPackedv3(
        channels = parallelChannels,
        kernelSize = kernelSize,
        depthOfLineBuffer = 64,
        depthOfBiasFifo = 512,
        depthOfWeightFifo = 512,
        depthOfUpdateBuffer = 4096
      )
    )
    compiled.doSimUntilVoid { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      //      SimTimeout(100000 * 10)

      val axiLiteSimThread = fork {
        val axiLiteDriver = AxiLite4Driver(dut.io.ctrl, dut.clockDomain)
        axiLiteDriver.reset()
        val pass = 8
        for (p <- 0 until pass) {

          val gen = new conv.SNNConvDataGen(inputChannels, outputChannels, timeStep, threshold, kernelSize, width, height)
          gen.genRandomData()
          val weights = gen.genWeightStream(parallelChannels, parallelChannels, paramBusBytes).flatten.map(_.toByte)
          val bias = gen.biasData.flatMap(s => utils.BigInt2ByteArray(s.toBigInt, 2))

          Array.copy(weights, 0, paramMem.mem, 0, weights.length)
          Array.copy(bias, 0, paramMem.mem, biasTranAddr.toInt, bias.length)

          val inputs = (0 until timeStep).toArray.map(i => gen.genInputStream(parallelChannels, i).flatten)
          val spikePacked = inputs.flatten.grouped(8).toArray.map(_.reverse.foldLeft(BigInt(0))((a, b) => (a << 1) + (b & 0x01))).map(_.toByte)
          Array.copy(spikePacked, 0, spikeMem.mem, mm2sBaseAddr.toInt, spikePacked.length)

          axiLiteDriver.write(0x00, configReg_0x00)
          axiLiteDriver.write(0x04, configReg_0x04)
          axiLiteDriver.write(0x08, configReg_0x08)
          axiLiteDriver.write(0x0C, configReg_0x0c)
          axiLiteDriver.write(0x20, configReg_0x20)
          axiLiteDriver.write(0x24, configReg_0x24)
          axiLiteDriver.write(0x28, configReg_0x28)
          axiLiteDriver.write(0x2C, configReg_0x2c)
          axiLiteDriver.write(0x30, configReg_0x30)
          axiLiteDriver.write(0x34, configReg_0x34)
          axiLiteDriver.write(0x38, configReg_0x38)
          axiLiteDriver.write(0x3C, configReg_0x3c)
          axiLiteDriver.write(0x40, configReg_0x40)
          axiLiteDriver.write(0x44, configReg_0x44)

          // print configReg in hex
          //          println("configReg_0x00: " + configReg_0x00.toHexString)
          //          println("configReg_0x04: " + configReg_0x04.toHexString)
          //          println("configReg_0x08: " + configReg_0x08.toHexString)
          //          println("configReg_0x0c: " + configReg_0x0c.toHexString)
          //          println("configReg_0x20: " + configReg_0x20.toHexString)
          //          println("configReg_0x24: " + configReg_0x24.toHexString)
          //          println("configReg_0x28: " + configReg_0x28.toHexString)
          //          println("configReg_0x2c: " + configReg_0x2c.toHexString)
          //          println("configReg_0x30: " + configReg_0x30.toHexString)
          //          println("configReg_0x34: " + configReg_0x34.toHexString)
          //          println("configReg_0x38: " + configReg_0x38.toHexString)
          //          println("configReg_0x3c: " + configReg_0x3c.toHexString)
          //          println("configReg_0x40: " + configReg_0x40.toHexString)
          //          println("configReg_0x44: " + configReg_0x44.toHexString)

          axiLiteDriver.write(0x48, 0x00010000L)
          dut.clockDomain.waitSampling(8)
          axiLiteDriver.write(0x48, 0x00010000L)
          dut.clockDomain.waitSampling(8)
          axiLiteDriver.write(0x48, 0x00000101L)

          while (axiLiteDriver.read(0x18) == 0) {
            dut.clockDomain.waitSampling(32)
          }
          axiLiteDriver.write(0x18, 0)

          dut.clockDomain.waitSampling(1024)
          val status = BigIntToListBoolean(axiLiteDriver.read(0x50), 64 bits)
          val mm2sBusy = status.head
          val s2mmBusy = status(8)
          val inputValid = status(16)
          val inputReady = status(24)
          val outputValid = status(32)
          val outputReady = status(40)
          val paramValid = status(48)
          val paramReady = status(56)

          // print all these status in a nice two dimensional table separated by "|"
          val statusTable = Array(
            Array("mm2sBusy", mm2sBusy), Array("s2mmBusy", s2mmBusy),
            Array("inputValid", inputValid), Array("inputReady", inputReady),
            Array("outputValid", outputValid), Array("outputReady", outputReady),
            Array("paramValid", paramValid), Array("paramReady", paramReady)
          )
          println(statusTable.map(_.mkString("\t")).mkString("  ", "  ", "  "))

          println(spikeMem.mm2sCmdQueue.size, spikeMem.mm2sCmdPendingQueue.size, spikeMem.s2mmCmdQueue.size, spikeMem.s2mmCmdPendingQueue.size)
          println(paramMem.mm2sCmdQueue.size, paramMem.mm2sCmdPendingQueue.size)

          val testOutputSpike = getFromSpikeMem
          val goldOutputSpike = if (enablePooling) gen.outputMaxPooling2x2() else gen.outputData
          val outWidth = if (enablePooling) width / 2 else width
          val outHeight = if (enablePooling) height / 2 else height
          for (t <- 0 until timeStep) {
            for (c <- 0 until outputChannels / parallelChannels) {
              for (p <- 0 until parallelChannels) {
                for (h <- 0 until outHeight) {
                  for (w <- 0 until outWidth) {
                    //                    val test = testOutputSpike(t)(c)(p)(h)(w)
                    // p w h c t
                    val test = testOutputSpike(p)(w)(h)(c)(t)
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
