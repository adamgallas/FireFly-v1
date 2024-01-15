package nnflow

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object LinkInputWeight {
  def apply[TI <: Data, TW <: Data](inputs: Stream[Fragment[TI]], weights: Stream[TW]) = {
    val ret = Stream(Fragment(utils.Linked(inputs.fragmentType, weights.payloadType)))
    val inputsHalted = inputs.haltWhen(!weights.valid)
    ret.arbitrationFrom(inputsHalted)
    ret.last := inputs.last
    ret.value := inputs.fragment
    ret.linked := weights.payload
    weights.ready := ret.fire && ret.last
    //    val weightsReady = Reg(Bool()) init False
    //    weightsReady := ret.fire && ret.last
    //    weightsReady.addAttribute("MAX_FANOUT", "500")
    //    weights.ready := weightsReady
    ret
  }
}
