import chisel3._
import chisel3.util._

/**
 * SRAM-Based Output System (Option C - TPU v1 Accumulator Buffer Architecture)
 *
 * Organizes accumulator storage into 'cols' independent SyncReadMem banks.
 * - Handles systolic skew naturally: Each column has its own write pointer.
 * - Conflict-free commit: Column (cols - 1) is the slowest column. When it writes,
 *   that row is committed across all banks.
 * - Parallel read: Emits a full unskewed row (Vec of cols) in a single cycle.
 * - Decoupled skid buffer: Absorbs the 1-cycle SyncReadMem read latency and provides
 *   clean zero-bubble backpressure handshakes.
 * - Dynamic stall generation: Signals upstream SystolicArray if memory approaches full.
 */
class SramOutputSystem(
  val rows: Int,
  val cols: Int,
  val depth: Int = 0, // 0 = automatic safe depth sizing
  config: TpuConfig
) extends Module {
  require(rows > 0 && cols > 0, "rows and cols must be positive integers")

  // Safe depth calculation:
  // Col 0 leads Col (cols - 1) by (cols - 1) cycles.
  // We need enough depth to hold the skew triangle plus rows in flight.
  val sramDepth = if (depth > 0) depth else 1 << log2Ceil((cols + rows + 4).max(8))
  val addrWidth = log2Ceil(sramDepth)

  val io = IO(new Bundle {
    // Inputs from Systolic Array bottom edge
    val in_res    = Input(Vec(cols, UInt(config.accWidth.W)))
    val in_en     = Input(Vec(cols, Bool()))

    // Stall signal to Systolic Array (gate computation when SRAM is near capacity)
    val stall     = Output(Bool())

    // Decoupled parallel row output (delivers a full row of 'cols' elements per handshake)
    val out_row   = Decoupled(Vec(cols, UInt(config.accWidth.W)))
  })

  // 1. Multi-bank on-chip SRAM (One SyncReadMem per column)
  val sramBanks = Seq.fill(cols)(SyncReadMem(sramDepth, UInt(config.accWidth.W)))

  // 2. Independent write pointer per column
  val wrPtrs = RegInit(VecInit(Seq.fill(cols)(0.U(addrWidth.W))))

  for (c <- 0 until cols) {
    when(io.in_en(c)) {
      sramBanks(c).write(wrPtrs(c), io.in_res(c))
      wrPtrs(c) := Mux(wrPtrs(c) === (sramDepth - 1).U, 0.U, wrPtrs(c) + 1.U)
    }
  }

  // 3. Row Completion & Commit Tracking
  // In a systolic array, Col (cols - 1) arrives latest for any row.
  // When Col (cols - 1) writes an element, that entire row is committed across all banks.
  val rowCommitted = io.in_en(cols - 1)

  // 2-entry skid buffer to absorb 1-cycle SyncReadMem read latency (flow = true for zero-bubble cut-through)
  val skidBuffer = Module(new Queue(Vec(cols, UInt(config.accWidth.W)), entries = 2, flow = true))

  val rdPtr         = RegInit(0.U(addrWidth.W))
  val committedRows = RegInit(0.U(log2Ceil(sramDepth + 1).W))
  val inFlightRead  = RegInit(false.B)

  // A read can be issued when:
  // - At least one full row is committed in SRAM
  // - The skid buffer has space to accept data on the next cycle
  val skidHasSpace = (skidBuffer.io.count + inFlightRead.asUInt) < 2.U
  val doRead       = (committedRows > 0.U) && skidHasSpace

  when(rowCommitted && !doRead) {
    committedRows := committedRows + 1.U
  } .elsewhen(!rowCommitted && doRead) {
    committedRows := committedRows - 1.U
  }

  // 4. Parallel read across all SRAM banks
  val rdata = Wire(Vec(cols, UInt(config.accWidth.W)))
  for (c <- 0 until cols) {
    rdata(c) := sramBanks(c).read(rdPtr, doRead)
  }

  when(doRead) {
    rdPtr := Mux(rdPtr === (sramDepth - 1).U, 0.U, rdPtr + 1.U)
  }

  inFlightRead := doRead

  // Enqueue read result into skid buffer
  skidBuffer.io.enq.valid := inFlightRead
  skidBuffer.io.enq.bits  := rdata

  // Connect skid buffer output to module interface
  io.out_row <> skidBuffer.io.deq

  // 5. Backpressure / Stall Detection
  // Col 0 is the fastest column. Distance between wrPtrs(0) and rdPtr determines fullness.
  val col0Occupancy = Wire(UInt(log2Ceil(sramDepth + 1).W))
  when(wrPtrs(0) >= rdPtr) {
    col0Occupancy := wrPtrs(0) - rdPtr
  } .otherwise {
    col0Occupancy := (sramDepth.U - rdPtr) + wrPtrs(0)
  }

  // Stall if within 2 rows of full capacity
  io.stall := col0Occupancy >= (sramDepth - 2).U
}
