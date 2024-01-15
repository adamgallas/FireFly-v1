import spinal.core._
import spinal.lib._

import java.lang.Package
import scala.language.postfixOps

case class UpdateMembrane(widthOfSum: Int, widthOfPartial: Int, channels: Int, signed: Boolean) extends Component {
  val io = new Bundle {
    val numOfIsPerO = in UInt (12 bits)
    val numOfOsPerClr = in UInt (12 bits)
    val threshold = in SInt (widthOfPartial bits)
    val winner_takes_all = in Bool() default False
    val leaky = in Bool() default False
    val offset = slave(Stream(Vec(Bits(widthOfSum bits), channels)))
    val extract = slave(Stream(Fragment(utils.Linked(Vec(Bits(widthOfPartial bits), channels), Vec(Bits(widthOfSum bits), channels)))))
    val update = master(Stream(Fragment(Vec(Bits(widthOfPartial bits), channels))))
    val results = master(Stream(Fragment(Vec(Bits(1 bits), channels))))
  }
  noIoPrefix()
  val cntIsPerOs = Counter(12 bits)
  val cntOsPerClr = Counter(12 bits)
  val clear = cntOsPerClr === 0 && cntIsPerOs === 0
  val resultsValid = cntIsPerOs === io.numOfIsPerO
  val about2clr = cntOsPerClr === io.numOfOsPerClr

  when(io.extract.fire && io.extract.last) {
    cntIsPerOs.increment()
    when(resultsValid) {
      cntIsPerOs.clear()
      cntOsPerClr.increment()
      when(about2clr) {
        cntOsPerClr.clear()
      }
    }
  }

  val resultsValidDly = RegNext(RegNext(resultsValid))
  val fifo = fifos.StreamFifoHighPerf(TupleBundle2(Fragment(Vec(Bits(widthOfPartial bits), channels)), Bool()), 8)

  val os = RegNext(RegNext(io.offset.payload))
  val curr = RegNext(io.extract.linked)
  val prev = RegNext(Mux(clear, Vec(B(0, widthOfPartial bits), channels), io.extract.value))
  if (signed) {
    val post = Vec((curr, prev).zipped.map((x, y) => RegNext(x.asSInt + y.asSInt)))
    val postOs = Vec((post, os).zipped.map((x, y) => RegNext(x + y.asSInt.andMask(resultsValidDly))))
    val postLeak = postOs.map(x => x >> io.leaky.asUInt)
    fifo.io.push._1.fragment.assignFromBits(postLeak.asBits)
  }
  else {
    val post = Vec((curr, prev).zipped.map((x, y) => RegNext(x.asUInt + y.asUInt)))
    val postOs = Vec((post, os).zipped.map((x, y) => RegNext(x + y.asUInt.andMask(resultsValidDly))))
    val postLeak = postOs.map(x => x >> io.leaky.asUInt)
    fifo.io.push._1.fragment.assignFromBits(postLeak.asBits)
  }
  fifo.io.push._1.last := Delay(io.extract.last, 3, init = False)
  fifo.io.push._2 := RegNext(resultsValidDly)
  fifo.io.push.valid := Delay(io.extract.fire, 3, init = False)
  io.extract.ready := fifo.io.pop.ready

  val stage = log2Up(channels)
  val maxVmem = fifo.io.pop._1.fragment.map(_.asSInt).reduceBalancedTree(_ max _, (s, l) => RegNext(s)) - 1

  val fifo2 = fifos.StreamFifoHighPerf(TupleBundle3(Fragment(Vec(Bits(widthOfPartial bits), channels)), Bool(), SInt(widthOfPartial bits)), 8)
  fifo2.io.push._1 := Delay(fifo.io.pop._1, stage)
  fifo2.io.push._2 := Delay(fifo.io.pop._2, stage)
  fifo2.io.push._3 := Mux(io.winner_takes_all, maxVmem, io.threshold)
  fifo2.io.push.valid := Delay(fifo.io.pop.fire, stage, init = False)
  fifo.io.pop.ready := fifo2.io.pop.ready

  val fifoPop = Stream(TupleBundle3(Fragment(Vec(Bits(widthOfPartial bits), channels)), Bool(), SInt(widthOfPartial bits)))
  fifoPop << fifo2.io.pop

  val ovf = Stream(TupleBundle3(Fragment(Vec(Bits(widthOfPartial bits), channels)), Vec(Bits(1 bits), channels), Bool()))
  ovf.arbitrationFrom(fifoPop)
  ovf._1 := fifoPop._1
  ovf._2 := Vec(ovf._1.map(x => (x.asSInt > fifoPop._3).asBits))
  ovf._3 := fifoPop._2

  val (toUpdate, toResult) = StreamFork2(ovf.m2sPipe())
  val voltage = toResult._1.fragment
  val integrateDone = toResult._3
  val isOverflow = toResult._2
  val last = toUpdate._1.last

  io.results.arbitrationFrom(toResult.throwWhen(!integrateDone))
  io.results.fragment := isOverflow
  io.results.last := last

  io.update.arbitrationFrom(toUpdate)
  io.update.fragment := Vec((voltage, isOverflow).zipped.map((v, isOvf) => Mux(integrateDone && !io.winner_takes_all, v.andMask(!isOvf.asBool), v)))
  io.update.last := last

  io.offset.ready := fifo.io.push.fire && fifo.io.push._1.last && fifo.io.push._2 && Delay(about2clr, 3)
}
