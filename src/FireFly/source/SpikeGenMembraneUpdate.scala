import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object SpikeGenMembraneUpdate {
  def apply(
             source: Stream[Fragment[Vec[Bits]]],
             offset: Stream[Vec[Bits]],
             numOfIsPerO: UInt, numOfOsPerClr: UInt, threshold: SInt,
             widthOfPartial: Int, signed: Boolean, depth: Int, tech: String = "auto",
             winner_takes_all: Bool = False,
             leaky:Bool=False
           ) = {
    val widthOfSum = source.fragment.head.getWidth
    val channels = source.fragment.length
    val acc = new SpikeGenMembraneUpdate(widthOfSum, widthOfPartial, channels, signed, depth, tech)
    acc.io.numOfIsPerO := numOfIsPerO.resized
    acc.io.numOfOsPerClr := numOfOsPerClr.resized
    acc.io.threshold := threshold.resized
    acc.io.winner_takes_all := winner_takes_all
    acc.io.leaky := leaky
    acc.io.offset << offset
    acc.io.source << source
    acc.io.dest
  }
}

class SpikeGenMembraneUpdate(widthOfSum: Int, widthOfPartial: Int, channels: Int, signed: Boolean, depth: Int, tech: String = "auto") extends Component {
  val io = new Bundle {
    val numOfIsPerO = in UInt (12 bits)
    val numOfOsPerClr = in UInt (12 bits)
    val threshold = in SInt (widthOfPartial bits)
    val winner_takes_all = in Bool() default False
    val leaky = in Bool() default False
    val offset = slave(Stream(Vec(Bits(widthOfSum bits), channels)))
    val source = slave(Stream(Fragment(Vec(Bits(widthOfSum bits), channels))))
    val dest = master(Stream(Fragment(Vec(Bits(1 bits), channels))))
  }
  noIoPrefix()

  val arith = UpdateMembrane(widthOfSum, widthOfPartial, channels, signed)
  val mem = utils.MemHistory(Vec(Bits(widthOfPartial bits), channels), Vec(Bits(widthOfSum bits), channels), depth, tech)

  mem.io.source << io.source
  mem.io.extract >> arith.io.extract
  mem.io.update << arith.io.update

  arith.io.offset << io.offset
  arith.io.numOfIsPerO := io.numOfIsPerO
  arith.io.numOfOsPerClr := io.numOfOsPerClr
  arith.io.threshold := io.threshold
  arith.io.winner_takes_all := io.winner_takes_all
  arith.io.results >> io.dest
  arith.io.leaky := io.leaky
}
