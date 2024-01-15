import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite._

import scala.language.postfixOps

case class AXIFireFlySys(
                          channels: Int,
                          kernelSize: Int,

                          depthOfLineBuffer: Int,
                          depthOfBiasFifo: Int,
                          depthOfWeightFifo: Int,
                          depthOfUpdateBuffer: Int,
                          largeBufferTech: String = "auto"
                        ) extends Component {

  val paramBusWidthReduceFactor = 2
  val inOutBusWidth = channels * 1
  val paramBusWidth = channels * 8 / paramBusWidthReduceFactor

  val io = new Bundle {
    val ctrl = slave(AxiLite4(32, 64))
    val inputs = slave(Stream(Bits(inOutBusWidth bits)))
    val params = slave(Stream(Fragment(Bits(paramBusWidth bits))))
    val outputs = master(Stream(Bits(inOutBusWidth bits)))

    val mm2sCmd = master(Stream(Bits(72 bits)))
    val s2mmCmd = master(Stream(Bits(72 bits)))
    val paramCmd = master(Stream(Bits(72 bits)))
  }
  noIoPrefix()
  AxiLite4SpecRenamer(io.ctrl)
  utils.AxiStreamSpecRenamer(io.inputs)
  utils.AxiStreamSpecRenamer(io.params)
  utils.AxiStreamSpecRenamer(io.outputs)
  utils.AxiStreamSpecRenamer(io.mm2sCmd)
  utils.AxiStreamSpecRenamer(io.s2mmCmd)
  utils.AxiStreamSpecRenamer(io.paramCmd)

  val numOfIFMs = UInt(8 bits) setAsReg() init 0
  val numOfOFMs = UInt(8 bits) setAsReg() init 0
  val numOfTimeSteps = UInt(8 bits) setAsReg() init 0
  val numOfTimeStepIFMs = UInt(12 bits) setAsReg() init 0
  val numOfTimeStepOFMs = UInt(12 bits) setAsReg() init 0
  val weightsLength = UInt(log2Up(depthOfWeightFifo) bits) setAsReg() init 0

  val width = UInt(log2Up(depthOfLineBuffer) bits) setAsReg() init 0
  val height = UInt(log2Up(depthOfLineBuffer) bits) setAsReg() init 0

  val threshold = SInt(18 bits) setAsReg() init 0
  val enablePooling = Bool() setAsReg() init false
  val directWidthAdapt = Bool() setAsReg() init false

  val configCtrl = new AxiLite4SlaveFactory(io.ctrl)

  configCtrl.write(numOfIFMs, 0x00, 0)
  configCtrl.write(numOfOFMs, 0x00, 8)
  configCtrl.write(numOfTimeSteps, 0x00, 16)
  configCtrl.write(numOfTimeStepIFMs, 0x00, 24)
  configCtrl.write(numOfTimeStepOFMs, 0x00, 36)
  configCtrl.write(enablePooling, 0x00, 48)
  configCtrl.write(directWidthAdapt, 0x00, 56)

  configCtrl.write(width, 0x08, 0)
  configCtrl.write(height, 0x08, 16)
  configCtrl.write(weightsLength, 0x08, 32)

  configCtrl.write(threshold, 0x10, 0)

  val conv = new FireFly(channels, kernelSize, depthOfLineBuffer, depthOfBiasFifo, depthOfWeightFifo, depthOfUpdateBuffer, largeBufferTech)

  conv.io.numOfIFMs := numOfIFMs
  conv.io.numOfOFMs := numOfOFMs
  conv.io.numOfTimeSteps := numOfTimeSteps
  conv.io.numOfTimeStepIFMs := numOfTimeStepIFMs
  conv.io.numOfTimeStepOFMs := numOfTimeStepOFMs
  conv.io.enablePooling := enablePooling
  conv.io.directWidthAdapt := directWidthAdapt

  conv.io.threshold := threshold
  conv.io.width := width
  conv.io.height := height
  conv.io.weightsLength := weightsLength

  conv.io.inputs << io.inputs
  conv.io.params << io.params
  io.outputs.arbitrationFrom(conv.io.outputs)
  io.outputs.payload := conv.io.outputs.payload

  val apdone = Bool() setAsReg() init false
  apdone setWhen conv.io.done
  configCtrl.read(apdone, 0x18, 0)
  configCtrl.onWrite(0x18)(apdone.clear())

  val mm2sCmdFifo = fifos.StreamFifoHighPerf(Bits(64 bits), 32)
  val s2mmCmdFifo = fifos.StreamFifoHighPerf(Bits(64 bits), 32)
  val paramCmdFifo = fifos.StreamFifoHighPerf(Bits(64 bits), 32)
  mm2sCmdFifo.io.push.valid.clear()
  s2mmCmdFifo.io.push.valid.clear()
  paramCmdFifo.io.push.valid.clear()
  mm2sCmdFifo.io.push.payload := io.ctrl.writeData.data
  s2mmCmdFifo.io.push.payload := io.ctrl.writeData.data
  paramCmdFifo.io.push.payload := io.ctrl.writeData.data

  configCtrl.read(mm2sCmdFifo.io.push.ready, 0x20, 0)
  configCtrl.read(s2mmCmdFifo.io.push.ready, 0x28, 0)
  configCtrl.read(paramCmdFifo.io.push.ready, 0x30, 0)
  configCtrl.onWrite(0x20)(mm2sCmdFifo.io.push.valid.set())
  configCtrl.onWrite(0x28)(s2mmCmdFifo.io.push.valid.set())
  configCtrl.onWrite(0x30)(paramCmdFifo.io.push.valid.set())

  io.mm2sCmd << mm2sCmdFifo.io.pop.translateWith(U"00000000" ## mm2sCmdFifo.io.pop.payload)
  io.s2mmCmd << s2mmCmdFifo.io.pop.translateWith(U"00000000" ## s2mmCmdFifo.io.pop.payload)
  io.paramCmd << paramCmdFifo.io.pop.translateWith(U"00000000" ## paramCmdFifo.io.pop.payload)
}
