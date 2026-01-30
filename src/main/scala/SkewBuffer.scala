import chisel3._
import chisel3.util._

class SkewBuffer(val rows: Int, config: TpuConfig) extends Module {
  val io = IO(new Bundle {
    val in  = Input(Vec(rows, UInt(config.dataWidth.W)))
    val out = Output(Vec(rows, UInt((config.dataWidth.W))))
  })

  for (i <- 0 until rows) {
    if (i == 0) {
      // Row 0 has no delay
      io.out(i) := io.in(i)
    } else {
      // Row i has 'i' cycles of delay
      io.out(i) := ShiftRegister(io.in(i), i)
    }
  }
}