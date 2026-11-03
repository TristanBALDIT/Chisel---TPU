import chisel3._
import chisel3.util._

class MemoryWriteController(val rows: Int, val cols: Int, config: TpuConfig) extends Module {
  val io = IO(new Bundle {
    val start       = Input(Bool())
    val baseAddr    = Input(UInt(32.W))
    val matrixWidth = Input(UInt(16.W))

    val in_res      = Input(Vec(cols, UInt(config.accWidth.W)))
    // FIX: Updated to Vec(cols, Bool()) to pass vector correctly
    val in_en       = Input(Vec(cols, Bool()))

    val mem_valid   = Output(Bool())
    val mem_ready   = Input(Bool())
    val mem_addr    = Output(UInt(32.W))
    val mem_data    = Output(UInt(config.accWidth.W))
  })

  // FIX: Updated to instantiate OutputSystemV2
  val outputSystem = Module(new OutputSystemV2(rows, cols, config))
  outputSystem.io.in_res := io.in_res
  outputSystem.io.in_en  := io.in_en

  val rowIdx = RegInit(0.U(16.W))
  val colIdx = RegInit(0.U(16.W))
  val currentAddr  = RegInit(0.U(32.W))
  val rowStartAddr = RegInit(0.U(32.W))

  outputSystem.io.out_stream.ready := io.mem_ready

  io.mem_valid := outputSystem.io.out_stream.valid
  io.mem_data  := outputSystem.io.out_stream.bits
  io.mem_addr  := currentAddr

  when(io.start) {
    currentAddr  := io.baseAddr
    rowStartAddr := io.baseAddr
    rowIdx := 0.U
    colIdx := 0.U
  } .elsewhen(io.mem_valid && io.mem_ready) {
    when(colIdx === (cols.U - 1.U)) {
      colIdx := 0.U
      rowIdx := rowIdx + 1.U
      val nextRowBase = rowStartAddr + (io.matrixWidth.pad(32) << log2Ceil(config.accWidth / 8)).asUInt
      currentAddr  := nextRowBase
      rowStartAddr := nextRowBase
    } .otherwise {
      colIdx := colIdx + 1.U
      currentAddr := currentAddr + (config.accWidth/8).U
    }
  }
}
