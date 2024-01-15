package utils

import spinal.core._
import spinal.lib._
import utils.LoopCounter

import scala.language.postfixOps

class StreamPadding[T <: Data](dataType: HardType[T], widthOfPadShape: List[Int], widthOfPad: List[Int]) extends Component {
  val io = new Bundle {
    val push = slave(Stream(dataType))
    val pop = master(Stream(dataType))
  }
  val config = new Bundle {
    val shape = in Vec widthOfPadShape.map(w => UInt(w bits))
    val padAtHead = in Vec widthOfPad.map(w => UInt(w bits))
    val padAtTail = in Vec widthOfPad.map(w => UInt(w bits))
  }
  noIoPrefix()
  config.setName("")

  val dummy = Stream(dataType)
  val dummyValid = Bool() setAsReg() init False
  dummy.payload := dataType().getZero
  dummy.valid := dummyValid

  val pad = Bool()
  val mux = new StreamMux(dataType(), 2)
  mux.io.select := pad.asUInt
  mux.io.inputs(0) << io.push
  mux.io.inputs(1) << dummy
  io.pop << mux.io.output

  val pushing = io.push.fire
  val popping = io.pop.fire

  val left = config.padAtHead
  val right = (config.padAtHead, config.shape).zipped.map(_ + _)
  val total = (right, config.padAtTail).zipped.map(_ + _ - 1)

  val loopCnt = new LoopCounter(widthOfPad)
  val cnt = loopCnt.io.cnt
  val cntOvf = loopCnt.io.cntOvf
  loopCnt.io.enable := popping
  loopCnt.io.bound := Vec(total)

  val leftCond = (left, cnt).zipped.map(_ <= _)
  val rightCond = (cnt, right).zipped.map(_ < _)
  val cond = (leftCond, rightCond).zipped.map(_ && _)
  pad := ~cond.reduce(_ && _)

  dummyValid.setWhen(io.push.valid)
  dummyValid.clearWhen(cntOvf.reduce(_ && _) && popping)
}
