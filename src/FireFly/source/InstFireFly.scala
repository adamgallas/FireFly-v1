import spinal.core._

import scala.language.postfixOps

object InstFireFly extends App {
  SpinalConfig(
    defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = LOW),
    nameWhenByFile = false,
    anonymSignalPrefix = "tmp"
  ).generateVerilog(
    new AXIFireFlyPackedv3(
      channels = 16,
      kernelSize = 3,
      depthOfLineBuffer = 512,
      depthOfBiasFifo = 512,
      depthOfWeightFifo = 1024,
      depthOfUpdateBuffer = 4096,
      largeBufferTech = "block"
    )
  )
}
