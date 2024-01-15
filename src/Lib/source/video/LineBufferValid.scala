package video

import spinal.core._
import spinal.lib._

object LineBufferValid {
  def apply[T <: Data](dataType: HardType[T], maxDepth: Int, lines: Int) = new LineBufferValid(dataType, maxDepth, lines)
}

class LineBufferValid[T <: Data](dataType: HardType[T], maxDepth: Int, lines: Int) extends Component {
  require(lines > 1)
  val io = new Bundle {
    val videoIn = slave(Stream(VideoPack(dataType)))
    val videoOut = master(Stream(VideoPack(Vec(dataType, lines))))
  }
  noIoPrefix()

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
  val feedbackAddCond = Bool()

  val in2outThrown = throwWhen(in2out, in2outThrowCond)
  val feedbackAdded = addWhen(feedback, feedbackAddCond)

  val in2fifoJoinFeedback = StreamJoin(in2fifo, feedbackAdded)
  val in2outJoinFifo2out = StreamJoin(in2outThrown, fifo2out)

  val ioVideoInFire = io.videoIn.fire

  val lineCnt = Counter(lines)
  val lineCntWontOverflowIfInc = !lineCnt.willOverflowIfInc
  when(ioVideoInFire && io.videoIn.frame.endOfFrame)(lineCnt.clear())
  when(ioVideoInFire && io.videoIn.frame.line.endOfLine && lineCntWontOverflowIfInc)(lineCnt.increment())

  in2outThrowCond := lineCntWontOverflowIfInc
  feedbackAddCond := lineCnt.value === 0

  fifo.io.push.arbitrationFrom(in2fifoJoinFeedback)
  fifo.io.push.payload := Vec(in2fifoJoinFeedback._2.drop(1) ++ Vec(in2fifoJoinFeedback._1.frame.line))
  fifo.io.flush := ioVideoInFire && io.videoIn.frame.endOfFrame

  io.videoOut.arbitrationFrom(in2outJoinFifo2out)
  io.videoOut.endOfPack := io.videoIn.endOfPack
  io.videoOut.frame.endOfFrame := io.videoIn.frame.endOfFrame
  io.videoOut.frame.line.pixel := Vec(in2outJoinFifo2out._2.map(_.pixel) ++ Vec(in2outJoinFifo2out._1.frame.line.pixel))
  io.videoOut.frame.line.endOfLine := io.videoIn.frame.line.endOfLine
}