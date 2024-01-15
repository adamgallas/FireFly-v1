import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axilite._

import scala.language.postfixOps

case class AXIFireFlyPackedv2(
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
  val winner_takes_all = Bool() setAsReg() init false

  val configCtrl = new AxiLite4SlaveFactory(io.ctrl)

  configCtrl.write(numOfIFMs, 0x00, 0)
  configCtrl.write(numOfOFMs, 0x00, 8)
  configCtrl.write(numOfTimeSteps, 0x00, 16)
  configCtrl.write(numOfTimeStepIFMs, 0x00, 24)
  configCtrl.write(numOfTimeStepOFMs, 0x00, 36)
  configCtrl.write(enablePooling, 0x00, 48)
  configCtrl.write(directWidthAdapt, 0x00, 56)
  configCtrl.write(winner_takes_all, 0x00, 60)

  configCtrl.write(width, 0x08, 0)
  configCtrl.write(height, 0x08, 16)
  configCtrl.write(weightsLength, 0x08, 32)
  configCtrl.readAndWrite(threshold, 0x10, 0)

  val conv = new FireFly(channels, kernelSize, depthOfLineBuffer, depthOfBiasFifo, depthOfWeightFifo, depthOfUpdateBuffer, largeBufferTech)

  conv.io.numOfIFMs := numOfIFMs
  conv.io.numOfOFMs := numOfOFMs
  conv.io.numOfTimeSteps := numOfTimeSteps
  conv.io.numOfTimeStepIFMs := numOfTimeStepIFMs
  conv.io.numOfTimeStepOFMs := numOfTimeStepOFMs
  conv.io.enablePooling := enablePooling
  conv.io.directWidthAdapt := directWidthAdapt

  conv.io.threshold := threshold
  conv.io.winner_takes_all := winner_takes_all
  conv.io.width := width
  conv.io.height := height
  conv.io.weightsLength := weightsLength

  conv.io.inputs << io.inputs
  conv.io.params << io.params
  io.outputs.arbitrationFrom(conv.io.outputs)
  io.outputs.payload := conv.io.outputs.payload

  val strideOfTimeStep = UInt(24 bits) setAsReg() init 0
  val strideOfChannel = UInt(24 bits) setAsReg() init 0
  val mm2sBaseAddr = UInt(32 bits) setAsReg() init 0
  val s2mmBaseAddr = UInt(32 bits) setAsReg() init 0
  val mm2sFixLen = UInt(23 bits) setAsReg() init 0
  val s2mmFixLen = UInt(23 bits) setAsReg() init 0

  val weightTranAddr = UInt(32 bits) setAsReg() init 0
  val weightTranLen = UInt(23 bits) setAsReg() init 0
  val biasTranAddr = UInt(32 bits) setAsReg() init 0
  val biasTranLen = UInt(23 bits) setAsReg() init 0

  val mm2sStartPulse = Bool() setAsReg() init false
  val s2mmStartPulse = Bool() setAsReg() init false
  val paramStartPulse = Bool() setAsReg() init false

  val afterInit = Bool() setAsReg() init false
  val convDone = conv.io.done.fall()
  afterInit setWhen paramStartPulse
  afterInit clearWhen convDone

  val apdone = Bool() setAsReg() init false
  apdone setWhen convDone && afterInit
  configCtrl.read(apdone, 0x18, 0)
  configCtrl.onWrite(0x18)(apdone.clear())

  configCtrl.write(strideOfTimeStep, 0x20, 0)
  configCtrl.write(strideOfChannel, 0x20, 32)
  configCtrl.write(mm2sBaseAddr, 0x28, 0)
  configCtrl.write(s2mmBaseAddr, 0x28, 32)
  configCtrl.write(mm2sFixLen, 0x30, 0)
  configCtrl.write(s2mmFixLen, 0x30, 32)
  configCtrl.write(weightTranAddr, 0x38, 0)
  configCtrl.write(weightTranLen, 0x38, 32)
  configCtrl.write(biasTranAddr, 0x40, 0)
  configCtrl.write(biasTranLen, 0x40, 32)

  configCtrl.write(mm2sStartPulse, 0x48, 0)
  configCtrl.write(s2mmStartPulse, 0x48, 16)
  configCtrl.write(paramStartPulse, 0x48, 32)
  mm2sStartPulse.clear()
  s2mmStartPulse.clear()
  paramStartPulse.clear()

  val mm2sCmdGen = utils.FixedLenStridedAddrCmdGen(
    widthOfBnd = List(numOfOFMs.getWidth),
    widthOfInc = List(0),
    addrWidth = 24, lenWidth = 23
  )
  mm2sCmdGen.io.bound(0) := numOfOFMs
  mm2sCmdGen.io.inc(0) := U(0)
  mm2sCmdGen.io.fixLen := mm2sFixLen
  mm2sCmdGen.io.startPulse := mm2sStartPulse

  val s2mmCmdGen = utils.FixedLenStridedAddrCmdGen(
    widthOfBnd = List(numOfTimeSteps.getWidth, numOfOFMs.getWidth),
    widthOfInc = List(24, 24),
    addrWidth = 24, lenWidth = 23
  )
  s2mmCmdGen.io.bound(0) := numOfTimeSteps
  s2mmCmdGen.io.bound(1) := numOfOFMs
  s2mmCmdGen.io.inc(0) := strideOfTimeStep
  s2mmCmdGen.io.inc(1) := strideOfChannel
  s2mmCmdGen.io.fixLen := s2mmFixLen
  s2mmCmdGen.io.startPulse := s2mmStartPulse

  configCtrl.read(Delay(mm2sCmdGen.io.busy, 4), 0x50, 0)
  configCtrl.read(Delay(s2mmCmdGen.io.busy, 4), 0x50, 8)
  configCtrl.read(Delay(io.inputs.valid, 4), 0x50, 16)
  configCtrl.read(Delay(io.inputs.ready, 4), 0x50, 24)
  configCtrl.read(Delay(io.outputs.valid, 4), 0x50, 32)
  configCtrl.read(Delay(io.outputs.ready, 4), 0x50, 40)
  configCtrl.read(Delay(io.params.valid, 4), 0x50, 48)
  configCtrl.read(Delay(io.params.ready, 4), 0x50, 56)

  val paramCmd = Stream(utils.Linked(UInt(32 bits), UInt(23 bits)))
  val paramCmdValid = Bool() setAsReg() init false
  val sel = Bool() setAsReg() init False
  paramCmdValid.clearWhen(paramCmd.ready).setWhen(paramStartPulse)
  sel.toggleWhen(paramStartPulse)
  paramCmd.valid := paramCmdValid
  paramCmd.value := Mux(sel, biasTranAddr, weightTranAddr)
  paramCmd.linked := Mux(sel, biasTranLen, weightTranLen)

  io.mm2sCmd << utils.GenAxiDataMoverCmd(mm2sCmdGen.io.cmd, mm2sBaseAddr, eof = True)
  io.s2mmCmd << utils.GenAxiDataMoverCmd(s2mmCmdGen.io.cmd, s2mmBaseAddr)
  io.paramCmd << utils.GenAxiDataMoverCmd(paramCmd, U(0), eof = True)
}
