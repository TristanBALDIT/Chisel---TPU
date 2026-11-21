import chisel3._
import chisel3.util._

/**
 * WeightLoader
 *
 * Controls the loading of the stationary Weight Matrix W (K x N) from an on-chip
 * Memory-Mapped RAM (e.g. Dual-Port Weight BRAM) into the top edge of the Systolic Array.
 *
 * Convention (C = A * W, where A is M x K and W is K x N, all row-major):
 * - Weight BRAM is stored row-major (no transpose needed):
 *     slot k (0 <= k < K) holds Row k of W = [W(k,0), W(k,1), ..., W(k,N-1)],
 *     which ends up in PE Row k.
 * - Counts down reqRow from (rows - 1) to 0 so slot 0 lands in PE Row 0
 *   and slot K-1 lands in PE Row K-1.
 * - Unused PE rows (r >= K) skip BRAM reads and inject invalid flush rows,
 *   clearing stale weights from previous runs.
 * - Unused PE cols (c >= N) are masked to 0 with out_w_valid = false.
 *
 * Can be instantiated either from a TpuConfig directly (`new WeightLoader(cfg)`)
 * or with explicit smaller dimensions for unit testing (`new WeightLoader(rows, cols, weightBramDepth, cfg)`).
 *
 * Sizing:
 * - matrixK width = log2Ceil(rows + 1)            (K = rows of W = active PE rows, K <= rows)
 * - matrixN width = log2Ceil(cols + 1)            (N = cols of W = active PE cols, N <= cols)
 * - mem_addr / baseAddr width = log2Ceil(weightBramDepth)
 */
class WeightLoader(
  val rows: Int,
  val cols: Int,
  val weightBramDepth: Int,
  val config: TpuConfig
) extends Module {
  def this(config: TpuConfig) = this(config.rows, config.cols, config.weightBramDepth, config)

  require(rows > 0 && cols > 0, "rows and cols must be positive integers")
  require(weightBramDepth >= rows.max(2), "weightBramDepth must be at least max(rows, 2)")

  val kWidth    = log2Ceil(rows + 1)
  val nWidth    = log2Ceil(cols + 1)
  val addrWidth = log2Ceil(weightBramDepth.max(2))

  val io = IO(new Bundle {
    // MMIO Control & Configuration
    val start       = Input(Bool())
    val baseAddr    = Input(UInt(addrWidth.W))
    val matrixK     = Input(UInt(kWidth.W)) // Rows of W = active PE rows (K <= rows)
    val matrixN     = Input(UInt(nWidth.W)) // Cols of W = active PE cols (N <= cols)
    val busy        = Output(Bool())
    val done        = Output(Bool())

    // Memory Interface (Connects to Port B of Weight BRAM)
    val mem_addr    = Output(UInt(addrWidth.W))
    val mem_en      = Output(Bool())
    val mem_data    = Input(Vec(cols, UInt(config.dataWidth.W)))

    // Flow Control
    val stall       = Input(Bool())

    // Direct interface to SystolicArray (top edge)
    val out_load    = Output(Bool())
    val out_w       = Output(Vec(cols, UInt(config.dataWidth.W)))
    val out_w_valid = Output(Vec(cols, Bool()))
  })

  // FSM States
  val sIdle :: sLoad :: sLastShift :: Nil = Enum(3)
  val state = RegInit(sIdle)

  // Shadow configuration registers latched at 'start'
  val baseAddrReg = RegInit(0.U(addrWidth.W))
  val kReg        = RegInit(0.U(kWidth.W))
  val nReg        = RegInit(0.U(nWidth.W))

  // Target row counter: counts down from (rows - 1) to 0
  // Width is log2Ceil(rows.max(2)), guaranteed <= addrWidth
  val reqRow = RegInit(0.U(log2Ceil(rows.max(2)).W))

  // Pipeline registers aligning 1-cycle BRAM read latency with mesh shift
  val loadActive = RegInit(false.B)
  val rowIsValid = RegInit(false.B)

  // Stage 1: Memory read request
  val inLoadStage   = (state === sLoad) && !io.stall
  val reqRowIsValid = reqRow < kReg
  val issuingRead   = inLoadStage && reqRowIsValid

  io.mem_addr := baseAddrReg + reqRow
  io.mem_en   := issuingRead

  // Stage 2: Drive SystolicArray top edge
  io.out_load := loadActive

  for (c <- 0 until cols) {
    val isElementValid = loadActive && rowIsValid && (c.U < nReg)
    io.out_w(c)       := Mux(isElementValid, io.mem_data(c), 0.U)
    io.out_w_valid(c) := isElementValid
  }

  // Status flags
  val doneReg = RegInit(false.B)
  io.done := doneReg
  io.busy := (state =/= sIdle)

  // FSM transitions
  switch(state) {
    is(sIdle) {
      doneReg    := false.B
      loadActive := false.B
      rowIsValid := false.B

      when(io.start) {
        baseAddrReg := io.baseAddr
        kReg        := io.matrixK
        nReg        := io.matrixN

        when(io.matrixK === 0.U || io.matrixN === 0.U) {
          // Zero-sized weight matrix: complete immediately
          doneReg := true.B
        } .otherwise {
          reqRow := (rows - 1).U
          state  := sLoad
        }
      }
    }

    is(sLoad) {
      when(!io.stall) {
        loadActive := true.B
        rowIsValid := reqRowIsValid

        when(reqRow === 0.U) {
          state := sLastShift
        } .otherwise {
          reqRow := reqRow - 1.U
        }
      }
    }

    is(sLastShift) {
      when(!io.stall) {
        loadActive := false.B
        rowIsValid := false.B
        doneReg    := true.B
        state      := sIdle
      }
    }
  }
}
