import chisel3._
import chisel3.util._

/**
 * ActivationController
 *
 * Controls the streaming of the Activation Matrix (V) from an on-chip
 * Memory-Mapped RAM (e.g. Dual-Port BRAM) into the Systolic Array.
 *
 * Features:
 * - Memory-Mapped registers: base address, vector count (M), and feature dimension (K).
 * - Automatic sub-matrix masking: generates valid tokens only for rows r < K.
 * - Integrated SkewBuffer: automatically staggers rows by their systolic skew delay.
 * - Full flow-control gating: freezes reading and skewing when downstream asserts 'stall'.
 * - Clean status signaling: 'busy' during execution, single-cycle 'done' pulse upon completion.
 */
class ActivationController(val rows: Int, config: TpuConfig) extends Module {
  val io = IO(new Bundle {
    // MMIO Control & Configuration
    val start       = Input(Bool())
    val baseAddr    = Input(UInt(32.W))
    val matrixM     = Input(UInt(16.W)) // Total vectors/timesteps (batch size or sequence length)
    val matrixK     = Input(UInt(16.W)) // Active rows (K <= rows)
    val busy        = Output(Bool())
    val done        = Output(Bool())

    // Memory Interface (Connects to Port B of Activation BRAM)
    val mem_addr    = Output(UInt(32.W))
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
  val baseAddrReg = RegInit(0.U(32.W))
  val mReg        = RegInit(0.U(16.W))
  val kReg        = RegInit(0.U(16.W))

  // Transfer counters
  val addrReg    = RegInit(0.U(32.W))
  val readCount  = RegInit(0.U(16.W))
  val drainCount = RegInit(0.U(log2Ceil(rows + 2).W))

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
        baseAddrReg := io.baseAddr
        mReg        := io.matrixM
        kReg        := io.matrixK
        addrReg     := io.baseAddr
        readCount   := 0.U
        drainCount  := 0.U

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
