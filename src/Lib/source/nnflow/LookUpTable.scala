package nnflow

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object LookUpTable {
  def apply(table: Stream[Bits], index: Stream[Vec[Bits]]) = {
    val lookUpTable = new LookUpTable(table.payload.getWidth, index.payload.length)
    lookUpTable.io.newTable << table
    lookUpTable.io.index << index
    lookUpTable.io.value
  }
}

class LookUpTable(width: Int, channels: Int) extends Component {
  val depth = scala.math.pow(2, width).toInt
  val io = new Bundle {
    val newTable = slave(Stream(Bits(width bits)))
    val index = slave(Stream(Vec(Bits(width bits), channels)))
    val value = master(Stream(Vec(Bits(width bits), channels)))
  }
  noIoPrefix()
  val ram = Mem(Bits(width bits), depth)
  ram.addAttribute("ram_style", "distributed")
  val cnt = Counter(width bits, inc = io.newTable.fire)
  ram.write(cnt, io.newTable.payload, io.newTable.fire)
  io.newTable.freeRun()

  val rData = io.index.payload.map(x => ram.readSync(x.asUInt, io.index.ready))
  val rValid = RegNextWhen(io.index.valid, io.index.ready) init False
  io.index.ready := io.value.ready
  io.index.ready setWhen (!rValid)
  io.value.valid := rValid
  io.value.payload := Vec(rData)
}
