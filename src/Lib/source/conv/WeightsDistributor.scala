package conv

import spinal.core._
import spinal.lib._

class WeightsDistributor[T <: Data](
                                     weightDataType: HardType[T],
                                     SIMDIn: Int,
                                     SIMDOut: Int,
                                     length: Int,
                                     weightsFifoDepth: Int
                                   ) extends Component {
  val io = new Bundle {
    val inputs = slave(Stream(Vec(Vec(weightDataType, SIMDOut), SIMDIn)))
    val outputs = master(Stream(Vec(Vec(Vec(weightDataType, SIMDOut), SIMDIn), length)))
  }
  noIoPrefix()

  //val lv1 = Stream(Vec(Vec(weightDataType, SIMDOut), SIMDIn))
  val lv2 = Stream(Vec(Vec(Vec(weightDataType, SIMDOut), SIMDIn), length))

  //StreamWidthAdapter(io.inputs, lv1)
  val fifo = fifos.StreamFifoHighPerf(Vec(Vec(weightDataType, SIMDOut), SIMDIn), weightsFifoDepth)
  utils.StreamBigWidthAdapter(fifo.io.pop, lv2, length)
  //lv1 >> fifo.io.push
  io.inputs >> fifo.io.push
  io.outputs << lv2.m2sPipe()
}
