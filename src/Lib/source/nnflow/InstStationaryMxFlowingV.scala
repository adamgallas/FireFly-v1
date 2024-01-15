package nnflow

import spinal.core._

import scala.language.postfixOps

object InstStationaryMxFlowingV extends App {
  SpinalConfig(
    defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
    targetDirectory = "data//verilog//Lib",
    anonymSignalPrefix = "tmp"
  ).generateVerilog(
    new StationaryMxFlowingV(
      widthOfInput = 8,
      widthOfWeight = 8,
      widthOfProduct = 16,
      widthOfSum = 24,
      dimension = (8 * 9, 8),
      SIMDIn = 1,
      SIMDOut = 2,
      stages = 4 + utils.log3Up(8 * 9),
      signed = true,
      xop = ArithmeticFactory.INT8Mult,
      aop = ArithmeticFactory.VecSIntReduce
    )
  )
}
