package tpu.input

import tpu.config._
import chisel3._
import chisel3.util._

/**
 * ActivationController
 *
 * Controls the streaming of the Activation Matrix A (M x K) from an on-chip
 * Memory-Mapped RAM (e.g. Dual-Port Activation BRAM) into the left edge
 * of the Systolic Array via the integrated SkewBuffer.
 *
 * Convention (C = A * W, where A is M x K and W is K x N, all row-major):
 * - Activation BRAM is stored row-major (no transpose needed):
 *     slot m (0 <= m < M) holds Row m of A = [A(m,0), A(m,1), ..., A(m,K-1)],
 *     streamed at timestep m.
 * - SkewBuffer delays PE Row k by k cycles so A(m, k) meets the vertical partial sum
 *   at the exact right cycle.
 *
 * Can be instantiated either from a TpuConfig directly (`new ActivationController(cfg)`)
 * or with explicit smaller dimensions for unit testing (`new ActivationController(rows, actBramDepth, cfg)`).
 *
 * Sizing:
 * - matrixM width = log2Ceil(actBramDepth + 1) (M = rows of A / batch = streaming timesteps, M <= actBramDepth)
 * - matrixK width = log2Ceil(rows + 1)         (K = cols of A = active PE rows, K <= rows)
 * - mem_addr / baseAddr width = log2Ceil(actBramDepth)
 */
class ActivationController(
  val rows: Int,
  val actBramDepth: Int,
  val config: TpuConfig
) extends Module {
  def this(config: TpuConfig) = this(config.rows, config.actBramDepth, config)

  require(rows > 0, "rows must be positive")
  require(actBramDepth >= 2, "actBramDepth must be at least 2")

  val kWidth    = log2Ceil(rows + 1)
  val mWidth    = log2Ceil(actBramDepth + 1)
  val addrWidth = log2Ceil(actBramDepth.max(2))

  val io = IO(new Bundle {
    // MMIO Control & Configuration
    val start       = Input(Bool())
    val baseAddr    = Input(UInt(addrWidth.W))
    val matrixM     = Input(UInt(mWidth.W)) // Rows of A (batch) = streaming timesteps (M <= actBramDepth)
    val matrixK     = Input(UInt(kWidth.W)) // Cols of A = active PE rows (K <= rows)
    val busy        = Output(Bool())
    val done        = Output(Bool())

    // Memory Interface (Connects to Port B of Activation BRAM)
    val mem_addr    = Output(UInt(addrWidth.W))
    val mem_en      = Output(Bool())
    val mem_data    = Input(Vec(rows, UInt(config.dataWidth.W)))

    // Flow Control (Backpressure from SramOutputSystem)
    val stall       = Input(Bool())

    // Direct interface to SystolicArray (left edge)
    val out_v       = Output(Vec(rows, UInt(config.dataWidth.W)))
    val out_v_valid = Output(Vec(rows, Bool()))
  })

  // FSM States
  val sIdle :: sFetch :: sDrain :: Nil = Enum(3)
  val state = RegInit(sIdle)

  // Shadow configuration registers latched at 'start'
  val mReg = RegInit(0.U(mWidth.W))
  val kReg = RegInit(0.U(kWidth.W))

  // Transfer counters
  val addrReg    = RegInit(0.U(addrWidth.W))
  val readCount  = RegInit(0.U(mWidth.W))
  val drainCount = RegInit(0.U(log2Ceil(rows + 1).W))

  // Integrated SkewBuffer
  val skewBuffer = Module(new SkewBuffer(rows, config))
  skewBuffer.io.stall := io.stall

  // Reading logic: issue read when in sFetch, not stalled, and vectors remain
  val issuingRead = (state === sFetch) && !io.stall && (readCount < mReg)

  // 1-cycle BRAM read latency tracking
  val rdataValid = RegInit(false.B)
  when(!io.stall) {
    rdataValid := issuingRead
  }

  // Memory interface connections
  io.mem_addr := addrReg
  io.mem_en   := issuingRead

  // Feed memory data and sub-matrix valid mask into SkewBuffer
  for (r <- 0 until rows) {
    val isRowValid = rdataValid && (r.U < kReg)
    skewBuffer.io.in(r)       := Mux(isRowValid, io.mem_data(r), 0.U)
    skewBuffer.io.in_valid(r) := isRowValid
  }

  // Drive Systolic Array inputs directly from SkewBuffer
  io.out_v       := skewBuffer.io.out
  io.out_v_valid := skewBuffer.io.out_valid

  // Status flags
  val doneReg = RegInit(false.B)
  io.done := doneReg
  io.busy := (state =/= sIdle)

  // FSM Transitions and Counter Logic
  switch(state) {
    is(sIdle) {
      doneReg := false.B
      when(io.start) {
        mReg       := io.matrixM
        kReg       := io.matrixK
        addrReg    := io.baseAddr
        readCount  := 0.U
        drainCount := 0.U

        when(io.matrixM === 0.U || io.matrixK === 0.U) {
          // Zero-sized matrix: complete immediately
          doneReg := true.B
        } .otherwise {
          state := sFetch
        }
      }
    }

    is(sFetch) {
      when(!io.stall) {
        addrReg   := addrReg + 1.U
        readCount := readCount + 1.U
        when(readCount + 1.U === mReg) {
          state := sDrain
        }
      }
    }

    is(sDrain) {
      when(!io.stall) {
        // Drain pipeline latency: 1 cycle (BRAM read latency) + (rows - 1) (SkewBuffer delay)
        drainCount := drainCount + 1.U
        when(drainCount === (rows - 1).U) {
          doneReg := true.B
          state   := sIdle
        }
      }
    }
  }
}
