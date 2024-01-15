package video

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class MaxPool2x2[T <: Data](dataType: HardType[T], maxWidth: Int, op: Vec[T] => T) extends Component {
  val io = new Bundle {
    val push = slave(Stream(dataType))
    val pop = master(Stream(dataType))
    val width = in UInt (log2Up(maxWidth) bits)
  }

  val widthCnt = Counter(log2Up(maxWidth) bits)
  val widthCntOvf = widthCnt === io.width
  val sel = Bool() setAsReg() init False
  when(io.push.fire) {
    widthCnt.increment()
    when(widthCntOvf) {
      sel := ~sel
      widthCnt.clear()
    }
  }

  val fifo = fifos.StreamFifoHighPerf(dataType, maxWidth)
  val pushBranch = StreamDemux(io.push, sel.asUInt, 2)
  val toFifo = pushBranch(0)
  val toOp = pushBranch(1)
  val fromFifo = fifo.io.pop
  val join = Event
  val joinPayload = Vec(fromFifo.payload, toOp.payload)
  val concatx2 = Stream(Vec(dataType, 2))
  val concatx4 = Stream(Vec(dataType, 4))

  join.valid := toOp.valid
  toOp.ready := join.ready
  fromFifo.ready := toOp.valid && join.ready

  toFifo >> fifo.io.push
  concatx2.arbitrationFrom(join)
  concatx2.payload := joinPayload
  StreamWidthAdapter(concatx2, concatx4)

  io.pop.arbitrationFrom(concatx4)
  io.pop.payload := op(concatx4.payload)
}
