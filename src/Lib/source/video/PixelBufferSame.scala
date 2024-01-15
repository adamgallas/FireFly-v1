package video

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object PixelBufferSame {
  def apply[T <: Data](dataType: HardType[T], depth: Int) = new PixelBufferSame(dataType, depth)
}

class PixelBufferSame[T <: Data](dataType: HardType[T], depth: Int) extends Component {
  require(depth % 2 == 1 && depth > 1)
  val halfDepth = (depth - 1) / 2
  val io = new Bundle {
    val videoIn = slave(Stream(VideoPack(dataType)))
    val videoOut = master(Stream(VideoPack(Vec(dataType, depth))))
  }
  noIoPrefix()

  val zero = dataType()
  zero.assignFromBits(B(dataType.getBitsWidth bits, default -> false))

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

  def counter(clear: Bool, inc: Bool, states: Int): (Bool, Bool) = {
    val cnt = Counter(states)
    val cond = Bool() setAsReg() init false setWhen clear clearWhen cnt.willOverflow
    when(clear)(cnt.clear())
    when(inc && cond)(cnt.increment())
    (cond, cnt.willOverflow)
  }

  val padded = Stream(VideoPack(dataType))
  val videoInFire = io.videoIn.fire
  val videoOutFire = io.videoOut.fire
  val paddedFire = padded.fire
  val (paddedCond, paddedLast) = counter(videoInFire && io.videoIn.endOfPack, paddedFire, halfDepth)
  padded << addWhen(io.videoIn, paddedCond)

  val pixelBuffer = (Bits(widthOf(dataType) * (depth - 1) bits)).setAsReg()
  when(paddedFire) {
    pixelBuffer := padded.frame.line.pixel ## (pixelBuffer >> widthOf(dataType))
  }
  val bridge = Stream(VideoPack(Vec(dataType, depth)))
  val bridgeFire = bridge.fire
  bridge.valid := padded.valid
  padded.ready := bridge.ready

  val cntPrev = UInt(log2Up(depth) bits) setAsReg() init halfDepth
  val cntPost = UInt(log2Up(depth) bits) setAsReg() init 0
  val cntPrevStable = cntPrev === halfDepth
  val cntPostStable = cntPost === halfDepth
  val cntPrevEqualVec = (for (i <- 0 until halfDepth) yield cntPrev >= i && cntPrev < halfDepth).reverse
  val cntPostEqualVec = (for (i <- 0 until halfDepth) yield cntPost <= i).reverse
  when(videoInFire && io.videoIn.frame.line.endOfLine)(cntPrev.clearAll())
  when(videoOutFire && io.videoOut.frame.line.endOfLine)(cntPost.clearAll())
  when(videoOutFire && !cntPrevStable)(cntPrev := cntPrev + 1)
  when(videoOutFire && !cntPostStable)(cntPost := cntPost + 1)

  val pixelVec = Vec(dataType, depth)
  pixelVec.assignFromBits(io.videoIn.frame.line.pixel ## pixelBuffer)
  val up = (cntPostEqualVec, pixelVec.take(halfDepth)).zipped.map(Mux(_, zero, _))
  val middle = IndexedSeq(pixelVec.drop(halfDepth).head)
  val down = (cntPrevEqualVec, pixelVec.takeRight(halfDepth)).zipped.map(Mux(_, zero, _))

  bridge.endOfPack := paddedLast
  bridge.frame.endOfFrame := Delay(padded.frame.endOfFrame, halfDepth, paddedFire, False)
  bridge.frame.line.endOfLine := Delay(padded.frame.line.endOfLine, halfDepth, paddedFire, False)
  bridge.frame.line.pixel := Vec(up ++ middle ++ down)

  val cnt = Counter(halfDepth + 1)
  val cntWontOverflowIfInc = !cnt.willOverflowIfInc
  when(bridgeFire && bridge.endOfPack)(cnt.clear())
  when(bridgeFire && cntWontOverflowIfInc)(cnt.increment())
  io.videoOut << throwWhen(bridge, cntWontOverflowIfInc)
}
