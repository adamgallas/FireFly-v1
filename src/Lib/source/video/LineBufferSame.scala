package video

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object LineBufferSame {
  def apply[T <: Data](dataType: HardType[T], maxDepth: Int, lines: Int) = new LineBufferSame(dataType, maxDepth, lines)
}

class LineBufferSame[T <: Data](dataType: HardType[T], maxDepth: Int, lines: Int) extends Component {
  require(lines % 2 == 1 && lines > 1)
  val halfLine = (lines - 1) / 2
  val io = new Bundle {
    val videoIn = slave(Stream(VideoPack(dataType)))
    val videoOut = master(Stream(VideoPack(Vec(dataType, lines))))
  }
  noIoPrefix()

  val zero = dataType()
  zero.assignFromBits(B(dataType.getBitsWidth bits, default -> false))

  def m2sPipePair[T_ <: Data](pair: (Stream[T_], Stream[T_])): (Stream[T_], Stream[T_]) = {
    (pair._1.m2sPipe(), pair._2.m2sPipe())
  }

  def throwWhen[T_ <: Data](streamIn: Stream[T_], cond: Bool): Stream[T_] = {
    val next = Stream(streamIn.payloadType)
    next << streamIn
    when(cond) {
      next.valid := False
      streamIn.ready := True
    }
    next
  }

  def addWhen[T_ <: Data](streamIn: Stream[T_], cond: Bool): Stream[T_] = {
    val next = Stream(streamIn.payloadType)
    next << streamIn
    when(cond) {
      next.valid := True
      streamIn.ready := False
    }
    next
  }

  val fifo = fifos.StreamFifoHighPerf(Vec(VideoLine(dataType), lines - 1), maxDepth)
  val (in2fifo, in2out) = StreamFork2(io.videoIn)
  val (feedback, fifo2out) = StreamFork2(fifo.io.pop)

  val in2outThrowCond = Bool()
  val in2outAddCond = Bool()
  val in2fifoAddCond = Bool()
  val fifo2outThrowCond = Bool()
  val feedbackThrowCond = Bool()
  val feedbackAddCond = Bool()

  val in2outThrownAdded = addWhen(throwWhen(in2out, in2outThrowCond), in2outAddCond)
  val in2fifoAdded = addWhen(in2fifo, in2fifoAddCond)
  val fifo2outThrown = throwWhen(fifo2out, fifo2outThrowCond)
  val feedbackThrownAdded = addWhen(throwWhen(feedback, feedbackThrowCond), feedbackAddCond)
  val in2fifoJoinFeedback = StreamJoin(in2fifoAdded, feedbackThrownAdded)
  val in2outJoinFifo2out = StreamJoin(in2outThrownAdded, fifo2outThrown)

  val firstFewLines = Counter(halfLine + 1)
  val ioVideoInFire = io.videoIn.fire
  val firstLine = firstFewLines === 0
  val firstFewLinesWontOverflowIfInc = !firstFewLines.willOverflowIfInc
  when(ioVideoInFire && io.videoIn.endOfPack)(firstFewLines.clear())
  when(ioVideoInFire && io.videoIn.frame.line.endOfLine && firstFewLinesWontOverflowIfInc)(firstFewLines.increment())

  val lastFewLines = Counter(halfLine)
  val ioVideoOutFire = io.videoOut.fire
  val lastFewLinesWontOverflowIfInc = !lastFewLines.willOverflowIfInc
  when(ioVideoOutFire && in2outJoinFifo2out._1.frame.endOfFrame)(lastFewLines.clear())
  when(ioVideoOutFire && in2outJoinFifo2out._2(halfLine).endOfLine)(lastFewLines.increment())

  val endOfFrameGap = Bool() setAsReg() init
    false setWhen (ioVideoInFire && io.videoIn.frame.endOfFrame) clearWhen (ioVideoOutFire && io.videoOut.frame.endOfFrame)
  val endOfPackGap = Bool() setAsReg() init
    false setWhen (ioVideoInFire && io.videoIn.endOfPack) clearWhen (ioVideoOutFire && io.videoOut.endOfPack)
  val endOfPackGapInv = !endOfPackGap

  in2outThrowCond := endOfPackGapInv && firstFewLinesWontOverflowIfInc
  feedbackAddCond := endOfPackGapInv && firstLine
  fifo2outThrowCond := endOfPackGapInv && !firstLine && firstFewLinesWontOverflowIfInc

  in2outAddCond := endOfPackGap
  feedbackThrowCond := endOfPackGap && lastFewLines.willOverflowIfInc
  in2fifoAddCond := endOfPackGap && lastFewLinesWontOverflowIfInc

  fifo.io.push.arbitrationFrom(in2fifoJoinFeedback)
  fifo.io.push.payload := Vec(in2fifoJoinFeedback._2.drop(1) ++ Vec(in2fifoJoinFeedback._1.frame.line))

  val cntPrev = UInt(log2Up(lines) bits) setAsReg() init halfLine
  val cntPost = UInt(log2Up(lines) bits) setAsReg() init 0
  val cntPrevStable = cntPrev === halfLine
  val cntPostStable = cntPost === halfLine
  val cntPrevEqualVec = (for (i <- 0 until halfLine) yield cntPrev >= i && cntPrev < halfLine).reverse
  val cntPostEqualVec = (for (i <- 0 until halfLine) yield cntPost <= i).reverse
  when(ioVideoInFire && io.videoIn.frame.endOfFrame)(cntPrev.clearAll())
  when(ioVideoOutFire && io.videoOut.frame.endOfFrame)(cntPost.clearAll())
  when(ioVideoOutFire && io.videoOut.frame.line.endOfLine && !cntPrevStable)(cntPrev := cntPrev + 1)
  when(ioVideoOutFire && io.videoOut.frame.line.endOfLine && !cntPostStable)(cntPost := cntPost + 1)

  val pixelVec = Vec(in2outJoinFifo2out._2.map(_.pixel) ++ Vec(in2outJoinFifo2out._1.frame.line.pixel))
  val up = (cntPostEqualVec, pixelVec.take(halfLine)).zipped.map(Mux(_, zero, _))
  val middle = IndexedSeq(pixelVec.drop(halfLine).head)
  val down = (cntPrevEqualVec, pixelVec.takeRight(halfLine)).zipped.map(Mux(_, zero, _))

  io.videoOut.arbitrationFrom(in2outJoinFifo2out)
  io.videoOut.endOfPack := endOfPackGap && io.videoOut.frame.line.endOfLine && lastFewLines.willOverflowIfInc
  io.videoOut.frame.endOfFrame := endOfFrameGap && io.videoOut.frame.line.endOfLine && lastFewLines.willOverflowIfInc
  io.videoOut.frame.line.pixel := Vec(up ++ middle ++ down)
  io.videoOut.frame.line.endOfLine := in2outJoinFifo2out._2(halfLine).endOfLine
}
