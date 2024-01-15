package conv

import spinal.core._
import spinal.lib._

object InputsWidthAdapt {
  def apply[T <: Data](dataType: HardType[T], kernelSize: Int, maxWidth: Int) = new InputsWidthAdapt(dataType, kernelSize, maxWidth)
}

class InputsWidthAdapt[T <: Data](dataType: HardType[T], kernelSize: Int, maxWidth: Int) extends Component {

  import video._

  val io = new Bundle {
    val directWidthAdapt = in Bool() default False
    val videoIn = slave(Stream(VideoPack(dataType)))
    val videoOut = master(Stream(VideoPack(Vec(Vec(dataType, kernelSize), kernelSize))))
  }
  noIoPrefix()

  val windowIn = Stream(VideoPack(dataType))
  val windowOut = Stream(VideoPack(Vec(Vec(dataType, kernelSize), kernelSize)))

  val widthAdaptIn = Stream(VideoPack(dataType))
  val widthAdaptOut = Stream(VideoPack(Vec(Vec(dataType, kernelSize), kernelSize)))

  val prePath = StreamDemux(io.videoIn, io.directWidthAdapt.asUInt, 2)
  io.videoOut << StreamMux(io.directWidthAdapt.asUInt, Vec(windowOut, widthAdaptOut)).s2mPipe().m2sPipe()
  prePath(0) >> windowIn
  prePath(1) >> widthAdaptIn

  val signal = widthAdaptIn.getSignal
  val payloadPre = widthAdaptIn.translateWith(widthAdaptIn.getPixel)
  val payloadPost = Stream(Vec(Vec(dataType, kernelSize), kernelSize))
  StreamWidthAdapter(payloadPre, payloadPost)
  widthAdaptOut.arbitrationFrom(payloadPost)
  widthAdaptOut.arbitrationSignalFrom(signal)
  widthAdaptOut.frame.line.pixel := payloadPost.payload

  val window = video.SlidingWindowSame(dataType, kernelSize, maxWidth)
  window.io.videoIn << windowIn
  window.io.videoOut >> windowOut
}
