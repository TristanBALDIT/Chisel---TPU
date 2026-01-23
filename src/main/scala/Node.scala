import chisel3._
import chisel3.util._

class Node(val w: Int) extends Module {
  val io = IO(new Bundle {
    val load      = Input(Bool())
    val en        = Input(Bool())
    val in_v      = Input(UInt(w.W))      // Activation
    val in_w      = Input(UInt((2*w).W))  // Weight (load) or partial sum (en)
    val out_v     = Output(UInt(w.W))
    val out_w_acc = Output(UInt((2*w).W)) // Propagated weight or new partial sum
  })

  val weight = RegInit(0.U(w.W))
  val vReg   = RegNext(io.in_v, 0.U)

  // Multiplication result (2*w bits)
  val mul = io.in_v * weight
  val next_w_acc = WireInit(0.U((2*w).W))

  when(io.load) {
    weight     := io.in_w(w-1, 0) // Capture lower bits for weight
    next_w_acc := weight          // Pass current weight down during load
  } .elsewhen(io.en) {
    next_w_acc := io.in_w + mul   // Streaming accumulation
  }

  // Pipeline register for the vertical path
  io.out_w_acc := RegNext(next_w_acc, 0.U)
  io.out_v     := vReg
}
