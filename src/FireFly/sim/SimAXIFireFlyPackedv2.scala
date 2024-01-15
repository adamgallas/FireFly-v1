import nnflow.ArithmeticFactory
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba4.axilite.sim.AxiLite4Driver
import spinal.lib.tools.BigIntToListBoolean
import utils.SimMemoryMap

import scala.language.postfixOps

object SimAXIFireFlyPackedv2 {

  val alwaysValid = false
  val alwaysReady = false

  val inputChannels = 16
  val outputChannels = 64
  val timeStep = 4
  val threshold = 64
  val enablePooling = true
  val directWidthAdapt = false
  val parallelChannels = 16
  val kernelSize = 3
  val width = 32
  val height = 32

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
  val mm2sFixLen = timeStep * width * height * inputChannels / 8
  val s2mmFixLen = outWidth * outHeight * parallelChannels / 8
  val weightTranLen = inputChannels * outputChannels * kernelSize * kernelSize
  val biasTranLen = outputChannels * 2

  val inputStrideOfTimeStep = width * height * inputChannels / 8
  val inputStrideOfChannel = width * height * parallelChannels / 8

  val outputStrideOfTimeStep = outWidth * outHeight * outputChannels / 8
  val outputStrideOfChannel = outWidth * outHeight * parallelChannels / 8

  val spikeMem = new SimMemoryMap(16 MiB, 8, 16)
  val paramMem = new SimMemoryMap(16 MiB, 8, 16)

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
    val compiled = SimConfig.withWave.allOptimisation.compile(
      rtl = AXIFireFlyPackedv2(
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
        val pass = 2
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
