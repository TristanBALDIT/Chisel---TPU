import chisel3._
import chisel3.util._

/**
 * Reverse-Skew Buffer + Static Parallel FIFOs (Option B)
 *
 * 1. Reverse-Skew Stage: A triangular shift-register delay line.
 *    Column c receives (cols - 1 - c) cycles of delay.
 *    This aligns the diagonal output wavefront into parallel rows in a single cycle.
 *
 * 2. Static Parallel FIFOs: Shallow FIFOs (e.g. depth 4-8) buffer the aligned rows
 *    to absorb downstream memory bus stalls and jitter.
 *
 * 3. Stall Detection: Generates a stall signal to the systolic array when the
 *    FIFOs reach a high-water mark.
 */
class ReverseSkewOutputSystem(
  val rows: Int,
  val cols: Int,
  val fifoDepth: Int = 8,
  config: TpuConfig
) extends Module {
  require(rows > 0 && cols > 0, "rows and cols must be positive integers")
  require(fifoDepth >= 4, "fifoDepth must be at least 4 to allow safe stall margin")

  val io = IO(new Bundle {
    val in_res    = Input(Vec(cols, UInt(config.accWidth.W)))
    val in_en     = Input(Vec(cols, Bool()))

    // Stall signal to upstream Systolic Array
    val stall     = Output(Bool())

    // Decoupled parallel row output
    val out_row   = Decoupled(Vec(cols, UInt(config.accWidth.W)))
  })

  // 1. Triangular Reverse-Skew Delay Line
  val deskewedData  = Wire(Vec(cols, UInt(config.accWidth.W)))
  val deskewedValid = Wire(Vec(cols, Bool()))

  for (c <- 0 until cols) {
    val delay = cols - 1 - c
    if (delay == 0) {
      deskewedData(c)  := io.in_res(c)
      deskewedValid(c) := io.in_en(c)
    } else {
      deskewedData(c)  := ShiftRegister(io.in_res(c), delay)
      deskewedValid(c) := ShiftRegister(io.in_en(c), delay)
    }
  }

  // A row is complete when all reverse-skewed valid signals align
  val rowValid = deskewedValid.reduce(_ && _)

  // 2. Parallel FIFOs (One per column, shallow depth)
  val queues = Seq.fill(cols)(Module(new Queue(UInt(config.accWidth.W), entries = fifoDepth)))

  for (c <- 0 until cols) {
    queues(c).io.enq.valid := rowValid
    queues(c).io.enq.bits  := deskewedData(c)
  }

  // 3. Parallel Dequeue Handshake
  val allDeqValid = queues.map(_.io.deq.valid).reduce(_ && _)
  val fireRow     = allDeqValid && io.out_row.ready

  for (c <- 0 until cols) {
    queues(c).io.deq.ready := fireRow
  }

  io.out_row.valid := allDeqValid
  io.out_row.bits  := VecInit(queues.map(_.io.deq.bits))

  // 4. Stall Detection (High-Water Mark)
  // All FIFOs fill in lockstep, so queues(0).io.count reflects the whole row buffer
  io.stall := queues(0).io.count >= (fifoDepth - 2).U
}
