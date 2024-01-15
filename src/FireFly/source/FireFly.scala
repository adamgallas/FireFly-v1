import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class FireFly(
               channels: Int,
               kernelSize: Int,

               depthOfLineBuffer: Int,
               depthOfBiasFifo: Int,
               depthOfWeightFifo: Int,
               depthOfUpdateBuffer: Int,
               largeBufferTech: String = "auto"
             ) extends Component {

  import nnflow._
  import utils._
  import fifos._

  val paramBusWidthReduceFactor = 2
  val inOutBusWidth = channels * 1
  val paramBusWidth = channels * 8 / paramBusWidthReduceFactor
  val kernelWinSize = kernelSize * kernelSize

  val weightLv1Factor = kernelWinSize * paramBusWidthReduceFactor
  val weightLv2Factor = channels

  val widthOfProduct = 12
  val widthOfSum = 16
  val widthOfPartial = 18

  val io = new Bundle {
    val numOfIFMs = in UInt (8 bits)
    val numOfOFMs = in UInt (8 bits)
    val numOfTimeSteps = in UInt (8 bits)
    val numOfTimeStepIFMs = in UInt (12 bits)
    val numOfTimeStepOFMs = in UInt (12 bits)
    val weightsLength = in UInt (log2Up(depthOfWeightFifo) bits)

    val width = in UInt (log2Up(depthOfLineBuffer) bits)
    val height = in UInt (log2Up(depthOfLineBuffer) bits)

    val threshold = in SInt (18 bits)
    val winner_takes_all = in Bool() default False
    val enablePooling = in Bool()
    val directWidthAdapt = in Bool() default False
    val leaky = in Bool() default False

    val inputs = slave(Stream(Bits(inOutBusWidth bits)))
    val params = slave(Stream(Fragment(Bits(paramBusWidth bits))))
    val outputs = master(Stream(Bits(inOutBusWidth bits)))
    val done = out Bool()
  }
  noIoPrefix()

  val postInputs = StreamFragmentReshape(
    ProcessInputs(
      io.inputs,
      io.width, io.height, io.numOfIFMs,
      io.directWidthAdapt, S(0, 8 bits),
      kernelWinSize, depthOfLineBuffer, needOffset = false
    ),
    Bits(1 bits),
    channels * kernelWinSize
  )
  val branch = StreamDemuxSwitchWhenLast(io.params, 2).map(StreamDropLast(_))
  val toBias = StreamWidthScaler(Queue(branch(0), depthOfBiasFifo), Vec(Bits(16 bits), channels))
  val toWeights = Cycle(
    StreamBigWidthAdapter(branch(1).s2mPipe().m2sPipe(), weightLv1Factor),
    io.numOfTimeSteps, io.weightsLength, depthOfWeightFifo, largeBufferTech
  )
  val toWeightsPipe = toWeights.m2sPipe()
  val postWeights = StreamReshape(
    StreamBigWidthAdapter.highPerf(toWeightsPipe, weightLv2Factor, channels * 2),
    Bits(8 bits), (channels, channels * kernelWinSize)
  )

  val sum = StationaryMxFlowingV(
    LinkInputWeight(postInputs, postWeights),
    widthOfProduct, widthOfSum,
    (2, 4), 1 + 2 + log3Up(channels * kernelWinSize / 2), signed = true,
    ArithmeticFactory.INT12MuxAddPipe, ArithmeticFactory.VecSIntReduce
  )
  val bias = toBias.translateWith(Vec(toBias.payload.map(_.asSInt.resize(widthOfSum).asBits)))
  val spike = SpikeGenMembraneUpdate(
    sum, bias, io.numOfIFMs, io.numOfTimeSteps, io.threshold, widthOfPartial,
    signed = true, depthOfUpdateBuffer, largeBufferTech, io.winner_takes_all, io.leaky
  )
  val postOutputs = ProcessOutputs(
    StreamDropLast(spike),
    Mux(io.directWidthAdapt, U(0), io.width),
    Mux(io.directWidthAdapt, U(0), io.height),
    io.numOfTimeStepOFMs, io.enablePooling, depthOfLineBuffer,
    (x: Vec[Bits], y: Vec[Bits]) => Vec((x, y).zipped.map((a, b) => (a(0) || b(0)).asBits))
  )
  io.outputs << postOutputs._1.transmuteWith(Bits(inOutBusWidth bits))
  io.done := postOutputs._2
}
