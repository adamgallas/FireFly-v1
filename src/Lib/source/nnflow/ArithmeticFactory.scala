package nnflow

import spinal.core._

import scala.language.postfixOps

object ArithmeticFactory {
  var useModel = false

  def setUseModel() = useModel = true

  def INT8Mult(input: Vec[Bits], weight: Vec[Vec[Bits]]): Vec[Bits] = {
    val SIMDIn = 1
    val SIMDOut = 2
    require(input.length == SIMDIn && weight.length == SIMDIn && weight(0).length == SIMDOut)

    val mult = xilinx.DSP48E2.APlusDMultB(useModel)
    val ret = Vec(Bits(16 bits), 2)
    mult.io.CEs.foreach(_.set())
    mult.io.A := weight(0)(0).asSInt.resize(30 bits).asBits
    mult.io.D := weight(0)(1).asSInt.expand ## B(27 - 9 bits, default -> false)
    mult.io.B := input(0).asSInt.resize(18 bits).asBits

    val ab = mult.io.P.take(16).asSInt
    val ac = mult.io.P.drop(18).take(16).asSInt
    ret(0) := ab.asBits
    ret(1) := Mux(ab(15), ac + 1, ac).asBits
    ret
  }

  def INT12MuxAdd(input: Vec[Bits], weight: Vec[Vec[Bits]]): Vec[Bits] = {
    val SIMDIn = 2
    val SIMDOut = 4
    require(input.length == SIMDIn && weight.length == SIMDIn && weight(0).length == SIMDOut)

    val muxAdd = xilinx.DSP48E2.FOUR12MuxAdd(useModel)
    muxAdd.io.CEs.foreach(_.set())
    muxAdd.io.AB := Vec(weight(0).map(_.asSInt.resize(12 bits))).asBits
    muxAdd.io.C := Vec(weight(1).map(_.asSInt.resize(12 bits))).asBits
    muxAdd.io.ABSel := input(0).asBool
    muxAdd.io.CSel := input(1).asBool
    Vec(muxAdd.io.P.subdivideIn(12 bits).map(_.asBits))
  }

  def INT12MuxAddPipe(input: Vec[Bits], weight: Vec[Vec[Bits]]): Vec[Bits] = {
    val SIMDIn = 2
    val SIMDOut = 4
    require(input.length == SIMDIn && weight.length == SIMDIn && weight(0).length == SIMDOut)

    val inputDly = RegNext(input)
    val weightDly = RegNext(weight)

    val muxAdd = xilinx.DSP48E2.FOUR12MuxAdd(useModel)
    muxAdd.io.CEs.foreach(_.set())
    muxAdd.io.AB := Vec(weightDly(0).map(_.asSInt.resize(12 bits))).asBits
    muxAdd.io.C := Vec(weightDly(1).map(_.asSInt.resize(12 bits))).asBits
    muxAdd.io.ABSel := inputDly(0).asBool
    muxAdd.io.CSel := inputDly(1).asBool
    Vec(muxAdd.io.P.subdivideIn(12 bits).map(_.asBits))
  }

  def INT16Scaler(input: Vec[Bits], scale: Bits, shift: Bits, zeroPoint: Bits, preShiftBits: Int = 16) = {
    val scaler = Array.fill(input.length)(xilinx.DSP48E2.ScalerINT16_8MULT(preShiftBits, input.head.getWidth, useModel))
    scaler.foreach(_.io.scalerBase := scale.asUInt)
    scaler.foreach(_.io.scalerExpr := shift.asUInt)
    scaler.foreach(_.io.CEs.foreach(_.set()))
    (scaler, input).zipped.foreach(_.io.din := _.asSInt)
    Vec(scaler.map(s => (s.io.dout + zeroPoint.asSInt).asBits))
  }

  def VecSIntReduce(input: Vec[Bits]) = {
    val op = (a: SInt, b: SInt, c: SInt) => a.expand + b.expand + c.expand
    val bridge = (s: SInt, _: Int) => RegNext(s)
    utils.ReduceTernaryTree(input.map(_.asSInt), op, bridge).asBits
  }

  def VecUIntReduce(input: Vec[Bits]) = {
    val op = (a: UInt, b: UInt, c: UInt) => a.expand + b.expand + c.expand
    val bridge = (s: UInt, _: Int) => RegNext(s)
    utils.ReduceTernaryTree(input.map(_.asUInt), op, bridge).asBits
  }
}
