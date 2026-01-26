import chisel3._
import chisel3.util._

class PE(config: TpuConfig) extends Module {
  val io = IO(new Bundle {
    val load      = Input(Bool())
    val en        = Input(Bool())
    val in_v      = Input(UInt(config.dataWidth.W))
    val in_w      = Input(UInt(config.dataWidth.W))
    val in_acc    = Input(UInt(config.accWidth.W))
    val out_v     = Output(UInt(config.dataWidth.W))
    val out_w     = Output(UInt(config.dataWidth.W))
    val out_acc   = Output(UInt(config.accWidth.W))
  })

  val weight = RegInit(0.U(config.dataWidth.W))
  val vReg   = RegNext(io.in_v, 0.U)

  io.out_w := weight

  // USE CONFIG MUL
  val product = config.multiply(io.in_v, weight)

  val next_acc = Wire(UInt(config.accWidth.W))

  when(io.load) {
    weight   := io.in_w
  } .elsewhen(io.en) {
    next_acc := io.in_acc + product
  } .otherwise {
    next_acc := io.in_acc
  }

  io.out_acc := RegNext(next_acc)
  io.out_v   := vReg
}
