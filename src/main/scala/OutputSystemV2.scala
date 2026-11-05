import chisel3._
import chisel3.util._

class OutputSystemV2(val rows: Int, val cols: Int, config: TpuConfig) extends Module {
  val io = IO(new Bundle {
    val in_res     = Input(Vec(cols, UInt(config.accWidth.W)))
    // FIX: Match SystolicArray's vectorized valid output
    val in_en      = Input(Vec(cols, Bool()))

    val out_stream = Decoupled(UInt(config.accWidth.W))
  })

  // 1. Skew-absorbing queues
  val queues = Seq.tabulate(cols) { colIdx =>
    val depth = (2 * rows - 1) + (cols - 1 - colIdx)
    Module(new Queue(UInt(config.accWidth.W), entries = depth))
  }

  for (i <- 0 until cols) {
    queues(i).io.enq.bits  := io.in_res(i)
    queues(i).io.enq.valid := io.in_en(i)
  }

  // 2. Output Serialization Logic (Completing the Output System)
  val colIdx = RegInit(0.U(16.W))

  // Map queue outputs into Vecs for dynamic indexing
  val queueDeqValid = VecInit(queues.map(_.io.deq.valid))
  val queueDeqBits  = VecInit(queues.map(_.io.deq.bits))

  io.out_stream.valid := queueDeqValid(colIdx)
  io.out_stream.bits  := queueDeqBits(colIdx)

  for (i <- 0 until cols) {
    // Only pop data from the currently active column queue
    queues(i).io.deq.ready := (colIdx === i.U) && io.out_stream.ready
  }

  // Advance to the next column when a successful handshake occurs
  when(io.out_stream.valid && io.out_stream.ready) {
    when(colIdx === (cols - 1).U) {
      colIdx := 0.U
    } .otherwise {
      colIdx := colIdx + 1.U
    }
  }
}