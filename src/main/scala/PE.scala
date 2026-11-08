import chisel3._
import chisel3.util._

class PE(config: TpuConfig) extends Module {
  val io = IO(new Bundle {
    // Control
    val stall        = Input(Bool())
    val load         = Input(Bool())

    // Vertical flow: Weights (shifted vertically during load)
    val in_w         = Input(UInt(config.dataWidth.W))
    val in_w_valid   = Input(Bool())
    val out_w        = Output(UInt(config.dataWidth.W))
    val out_w_valid  = Output(Bool())

    // Horizontal flow: Activations (streamed horizontally from the left)
    val in_v         = Input(UInt(config.dataWidth.W))
    val in_v_valid   = Input(Bool())
    val out_v        = Output(UInt(config.dataWidth.W))
    val out_v_valid  = Output(Bool())

    // Vertical flow: Accumulator / Partial Sums (accumulated down the column)
    val in_acc       = Input(UInt(config.accWidth.W))
    val in_acc_valid = Input(Bool())
    val out_acc      = Output(UInt(config.accWidth.W))
    val out_acc_valid = Output(Bool())
  })

  // Registers for weight and active validity
  val weight       = RegInit(0.U(config.dataWidth.W))
  val weight_valid = RegInit(false.B)

  // Pipeline registers for horizontal propagation
  val vReg         = RegInit(0.U(config.dataWidth.W))
  val vValidReg    = RegInit(false.B)

  // Pipeline registers for vertical accumulator propagation
  val accReg       = RegInit(0.U(config.accWidth.W))
  val accValidReg  = RegInit(false.B)

  // Multiplication using TPU config definition
  val product = config.multiply(io.in_v, weight)

  // Compute condition: activation is valid AND weight is valid
  val compute = io.in_v_valid && weight_valid

  // Next accumulator calculation:
  // If active computation: add product to in_acc.
  // Otherwise: pass in_acc straight through (bypass / transparent shift).
  val next_acc = Wire(UInt(config.accWidth.W))
  when(compute) {
    next_acc := io.in_acc + product
  } .otherwise {
    next_acc := io.in_acc
  }

  // Next accumulator validity:
  // Valid if either:
  // 1. This PE performed a valid MAC operation (compute is true)
  // 2. An already valid partial sum from an upper row is passing through (io.in_acc_valid is true)
  val next_acc_valid = compute || io.in_acc_valid

  // Pipeline register updates (freeze completely when stalled)
  when(!io.stall) {
    when(io.load) {
      weight       := io.in_w
      weight_valid := io.in_w_valid
      accReg       := 0.U
      accValidReg  := false.B
    } .otherwise {
      accReg       := next_acc
      accValidReg  := next_acc_valid
    }

    // Horizontal activation streaming
    vReg      := io.in_v
    vValidReg := io.in_v_valid
  }

  // Drive outputs
  io.out_w       := weight
  io.out_w_valid := weight_valid

  io.out_v       := vReg
  io.out_v_valid := vValidReg

  io.out_acc       := accReg
  io.out_acc_valid := accValidReg
}
