import chisel3._
import chisel3.util._

class Node(val w: Int) extends Module {
  val io = IO(new Bundle {
    val ctrl_en = Input(Bool())

    val in_a    = Input(UInt(w.W))
    val in_b    = Input(UInt(w.W))

    val out_a   = Output(UInt(w.W))
    val out_b   = Output(UInt(w.W))

    val res     = Output(UInt((2*w).W))
  })

  val aReg = RegNext(io.in_a, 0.U)
  val bReg = RegNext(io.in_b, 0.U)


  val acc = RegInit(0.U((2*w).W))

  when(io.ctrl_en) {
    // TODO custom mul
    acc := acc + (io.in_a * io.in_b)
  }

  io.out_a := aReg
  io.out_b := bReg
  io.res   := acc
}
