package video

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object VideoFrameHistory {
  def apply[TI <: Data, T <: Data, TO <: Data](
                                                numberOfInputFrames: UInt,
                                                numberOfOutputFrames: UInt,
                                                offset: Stream[T],
                                                videoIn: Stream[VideoPack[TI]],
                                                videoOut: Stream[VideoPack[TO]],
                                                bufferSize: Int,
                                                op: (TI, T, T, Bool, Bool) => (T, TO),
                                                tech: String = "block"
                                              ) = {


    val ram = Mem(offset.payloadType, bufferSize)
    ram.addAttribute("ram_style", tech)

    val videoInPipe = videoIn.m2sPipe()
    val inPixelCnt = Counter(log2Up(bufferSize) bits)
    when(videoIn.fire)(inPixelCnt.increment())
    when(inPixelCnt.willIncrement && videoIn.frame.endOfFrame)(inPixelCnt.clear())

    val firstFrame = Bool() setAsReg() init true
    val frameCnt = Counter(8 bits)
    val lastFrame = frameCnt === numberOfInputFrames

    val videoInPipeFire = videoInPipe.fire
    firstFrame.clearWhen(videoInPipe.frame.endOfFrame && videoInPipeFire)
    firstFrame.setWhen(videoInPipe.endOfPack && videoInPipeFire)

    val prevData = ram.readSync(inPixelCnt, videoIn.ready)
    val currData = videoInPipe.frame.line.pixel
    val postData = op(currData, prevData, offset.payload, firstFrame, lastFrame)

    val bridge = Stream(VideoPack(videoOut.frame.line.dataType))
    val bridgePipe = bridge.m2sPipe()
    val bridgePipeFire = bridgePipe.fire
    bridge.arbitrationFrom(videoInPipe)
    bridge.endOfPack := videoInPipe.endOfPack
    bridge.frame.endOfFrame := videoInPipe.frame.endOfFrame
    bridge.frame.line.endOfLine := videoInPipe.frame.line.endOfLine
    bridge.frame.line.pixel := postData._2

    when(bridgePipeFire && bridgePipe.frame.endOfFrame)(frameCnt.increment())
    when(frameCnt.willIncrement && lastFrame)(frameCnt.clear())

    val outPixelCnt = Counter(log2Up(bufferSize) bits)
    when(bridgePipeFire)(outPixelCnt.increment())
    when(outPixelCnt.willIncrement && bridgePipe.frame.endOfFrame)(outPixelCnt.clear())
    ram.write(outPixelCnt, RegNextWhen(postData._1, bridgePipe.ready), bridgePipeFire)

    val packCnt = Counter(8 bits)
    val lastPack = packCnt === numberOfOutputFrames
    when(bridgePipeFire && bridgePipe.endOfPack)(packCnt.increment())
    when(packCnt.willIncrement && lastPack)(packCnt.clear())

    videoOut.arbitrationFrom(bridgePipe.throwWhen(!lastFrame))
    videoOut.endOfPack := bridgePipe.endOfPack && lastPack
    videoOut.frame.endOfFrame := bridgePipe.frame.endOfFrame
    videoOut.frame.line.endOfLine := bridgePipe.frame.line.endOfLine
    videoOut.frame.line.pixel := bridgePipe.frame.line.pixel

    offset.ready := videoOut.frame.endOfFrame && videoOut.fire
  }
}

class VideoFrameHistory[TI <: Data, T <: Data, TO <: Data](
                                                            inDataType: HardType[TI],
                                                            tempDataType: HardType[T],
                                                            outDataType: HardType[TO],
                                                            bufferSize: Int,
                                                            op: (TI, T, T, Bool, Bool) => (T, TO),
                                                            tech: String = "block"
                                                          ) extends Component {
  val io = new Bundle {
    val numberOfInputFrames = in UInt (8 bits)
    val numberOfOutputFrames = in UInt (8 bits)
    val offset = slave(Stream(tempDataType))
    val videoIn = slave(Stream(VideoPack(inDataType)))
    val videoOut = master(Stream(VideoPack(outDataType)))
  }
  noIoPrefix()

  val ram = Mem(tempDataType, bufferSize)

  val videoInPipe = io.videoIn.m2sPipe()
  val inPixelCnt = Counter(log2Up(bufferSize) bits)
  when(io.videoIn.fire)(inPixelCnt.increment())
  when(inPixelCnt.willIncrement && io.videoIn.frame.endOfFrame)(inPixelCnt.clear())

  val firstFrame = Bool() setAsReg() init true
  val frameCnt = Counter(8 bits)
  val lastFrame = frameCnt === io.numberOfInputFrames

  val videoInPipeFire = videoInPipe.fire
  firstFrame.clearWhen(videoInPipe.frame.endOfFrame && videoInPipeFire)
  firstFrame.setWhen(videoInPipe.endOfPack && videoInPipeFire)

  val prevData = ram.readSync(inPixelCnt, io.videoIn.ready)
  val currData = videoInPipe.frame.line.pixel
  val postData = op(currData, prevData, io.offset.payload, firstFrame, lastFrame)

  val bridge = Stream(VideoPack(outDataType))
  val bridgePipe = bridge.m2sPipe()
  val bridgePipeFire = bridgePipe.fire
  bridge.arbitrationFrom(videoInPipe)
  bridge.endOfPack := videoInPipe.endOfPack
  bridge.frame.endOfFrame := videoInPipe.frame.endOfFrame
  bridge.frame.line.endOfLine := videoInPipe.frame.line.endOfLine
  bridge.frame.line.pixel := postData._2

  when(bridgePipeFire && bridgePipe.frame.endOfFrame)(frameCnt.increment())
  when(frameCnt.willIncrement && lastFrame)(frameCnt.clear())

  val outPixelCnt = Counter(log2Up(bufferSize) bits)
  when(bridgePipeFire)(outPixelCnt.increment())
  when(outPixelCnt.willIncrement && bridgePipe.frame.endOfFrame)(outPixelCnt.clear())
  ram.write(outPixelCnt, RegNextWhen(postData._1, bridgePipe.ready), bridgePipeFire)

  val packCnt = Counter(8 bits)
  val lastPack = packCnt === io.numberOfOutputFrames
  when(bridgePipeFire && bridgePipe.endOfPack)(packCnt.increment())
  when(packCnt.willIncrement && lastPack)(packCnt.clear())

  io.videoOut.arbitrationFrom(bridgePipe.throwWhen(!lastFrame))
  io.videoOut.endOfPack := bridgePipe.endOfPack && lastPack
  io.videoOut.frame.endOfFrame := bridgePipe.frame.endOfFrame
  io.videoOut.frame.line.endOfLine := bridgePipe.frame.line.endOfLine
  io.videoOut.frame.line.pixel := bridgePipe.frame.line.pixel

  io.offset.ready := io.videoOut.frame.endOfFrame && io.videoOut.fire
}
