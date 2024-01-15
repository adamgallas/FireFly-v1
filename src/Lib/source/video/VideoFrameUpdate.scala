package video

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object VideoFrameUpdate {
  def apply[TI <: Data, T <: Data, TO <: Data](
                                                numOfIFMsPerOFM: UInt,
                                                offset: Stream[T],
                                                videoIn: Stream[VideoPack[TI]],
                                                videoOut: Stream[VideoPack[TO]],
                                                bufferSize: Int,
                                                stages: Int,
                                                op: (TupleBundle5[TI, T, T, Bool, Bool], Vec[Bool]) => TupleBundle2[T, TO],
                                                tech: String = "block",
                                                passThrough: Boolean = false
                                              ) = {
    if (passThrough) {
      val inDataType = videoIn.frame.line.dataType
      val updataType = offset.payloadType
      val outDataType = videoOut.frame.line.dataType

      val curr = videoIn.frame.line.pixel
      val prev = offset.payload.getZero

      val updateData = TupleBundle5(inDataType, updataType, updataType, Bool, Bool)
      updateData._1 := curr
      updateData._2 := prev
      updateData._3 := offset.payload
      updateData._4 := True
      updateData._5 := True

      val updateVideoIn = Stream(VideoPack(TupleBundle5(inDataType, updataType, updataType, Bool, Bool)))
      val updateVideoOut = Stream(VideoPack(TupleBundle2(updataType, outDataType)))
      VideoMultiStageLogic(updateVideoIn, updateVideoOut, stages, op)
      updateVideoIn << videoIn.translateWith(videoIn.translatePixelWith(updateData))

      offset.ready := updateVideoOut.endOfPack && updateVideoOut.fire

      videoOut.arbitrationFrom(updateVideoOut.throwWhen(!True))
      videoOut.endOfPack := updateVideoOut.endOfPack
      videoOut.frame.endOfFrame := updateVideoOut.frame.endOfFrame
      videoOut.frame.line.endOfLine := updateVideoOut.frame.line.endOfLine
      videoOut.frame.line.pixel := updateVideoOut.frame.line.pixel._2
    }
    else {
      val inDataType = videoIn.frame.line.dataType
      val updataType = offset.payloadType
      val outDataType = videoOut.frame.line.dataType

      val ram = Mem(updataType, bufferSize)
      ram.addAttribute("ram_style", tech)
      val videoInPipe = videoIn.m2sPipe()
      val videoInPipeFire = videoInPipe.fire

      val inPixelCnt = Counter(log2Up(bufferSize) bits)
      val outPixelCnt = Counter(log2Up(bufferSize) bits)
      val fmCntPre = Counter(8 bits)
      val fmCntPost = Counter(8 bits)

      val firstIFM = Bool() setAsReg() init true
      firstIFM.clearWhen(videoInPipe.frame.endOfFrame && videoInPipeFire)
      firstIFM.setWhen(videoInPipe.endOfPack && videoInPipeFire)

      val numOfIFMsPerOFMReachedPre = fmCntPre === numOfIFMsPerOFM
      val numOfIFMsPerOFMReachedPost = fmCntPost === numOfIFMsPerOFM

      val curr = videoInPipe.frame.line.pixel
      val prev = ram.readSync(inPixelCnt, videoIn.ready)

      val updateData = TupleBundle5(inDataType, updataType, updataType, Bool, Bool)
      updateData._1 := curr
      updateData._2 := prev
      updateData._3 := offset.payload
      updateData._4 := firstIFM
      updateData._5 := numOfIFMsPerOFMReachedPre

      val updateVideoIn = Stream(VideoPack(TupleBundle5(inDataType, updataType, updataType, Bool, Bool)))
      val updateVideoOut = Stream(VideoPack(TupleBundle2(updataType, outDataType)))
      VideoMultiStageLogic(updateVideoIn, updateVideoOut, stages, op)
      updateVideoIn << videoInPipe.translateWith(videoInPipe.translatePixelWith(updateData))

      // frame cnt
      when(updateVideoIn.fire && updateVideoIn.frame.endOfFrame)(fmCntPre.increment())
      when(fmCntPre.willIncrement && numOfIFMsPerOFMReachedPre)(fmCntPre.clear())

      when(updateVideoOut.fire && updateVideoOut.frame.endOfFrame)(fmCntPost.increment())
      when(fmCntPost.willIncrement && numOfIFMsPerOFMReachedPost)(fmCntPost.clear())

      // pixel cnt
      when(videoIn.fire)(inPixelCnt.increment())
      when(inPixelCnt.willIncrement && videoIn.frame.endOfFrame)(inPixelCnt.clear())

      when(updateVideoOut.fire)(outPixelCnt.increment())
      when(outPixelCnt.willIncrement && updateVideoOut.frame.endOfFrame)(outPixelCnt.clear())
      ram.write(outPixelCnt, updateVideoOut.frame.line.pixel._1, updateVideoOut.fire)

      offset.ready := updateVideoOut.endOfPack && updateVideoOut.fire

      videoOut.arbitrationFrom(updateVideoOut.throwWhen(!numOfIFMsPerOFMReachedPost))
      videoOut.endOfPack := updateVideoOut.endOfPack
      videoOut.frame.endOfFrame := updateVideoOut.frame.endOfFrame
      videoOut.frame.line.endOfLine := updateVideoOut.frame.line.endOfLine
      videoOut.frame.line.pixel := updateVideoOut.frame.line.pixel._2
    }
  }
}

class VideoFrameUpdate[TI <: Data, T <: Data, TO <: Data](
                                                           inDataType: HardType[TI],
                                                           updataType: HardType[T],
                                                           outDataType: HardType[TO],
                                                           bufferSize: Int,
                                                           stages: Int,
                                                           op: (TupleBundle5[TI, T, T, Bool, Bool], Vec[Bool]) => TupleBundle2[T, TO],
                                                           tech: String = "block",
                                                           passThrough: Boolean = false
                                                         ) extends Component {
  val io = new Bundle {
    val numOfIFMsPerOFM = in UInt (8 bits)
    val offset = slave(Stream(updataType))
    val videoIn = slave(Stream(VideoPack(inDataType)))
    val videoOut = master(Stream(VideoPack(outDataType)))
  }
  noIoPrefix()
  VideoFrameUpdate(io.numOfIFMsPerOFM, io.offset, io.videoIn, io.videoOut, bufferSize, stages, op, tech, passThrough)
}