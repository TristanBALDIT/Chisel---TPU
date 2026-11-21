import chisel3._
import chisel3.util._

class SkewBuffer(val rows: Int, val config: TpuConfig) extends Module {
  def this(config: TpuConfig) = this(config.rows, config)
  require(rows > 0, "rows must be a positive integer")

  val io = IO(new Bundle {
    val stall     = Input(Bool())
    val in        = Input(Vec(rows, UInt(config.dataWidth.W)))
    val in_valid  = Input(Vec(rows, Bool()))
    val out       = Output(Vec(rows, UInt(config.dataWidth.W)))
    val out_valid = Output(Vec(rows, Bool()))
  })

  for (i <- 0 until rows) {
    if (i == 0) {
      // Row 0 has no delay
      io.out(i)       := io.in(i)
      io.out_valid(i) := io.in_valid(i)
    } else {
      // Row i has 'i' cycles of delay (shifts in lockstep, freezes when stalled)
      val dataRegs  = RegInit(VecInit(Seq.fill(i)(0.U(config.dataWidth.W))))
      val validRegs = RegInit(VecInit(Seq.fill(i)(false.B)))

      when(!io.stall) {
        dataRegs(0)  := io.in(i)
        validRegs(0) := io.in_valid(i)
        for (j <- 1 until i) {
          dataRegs(j)  := dataRegs(j - 1)
          validRegs(j) := validRegs(j - 1)
        }
      }

      io.out(i)       := dataRegs(i - 1)
      io.out_valid(i) := validRegs(i - 1)
    }
  }
}
