import chisel3._
import chisel3.util._

class OutputSystem(val cols: Int, config: TpuConfig) extends Module {
  val io = IO(new Bundle {


    val in_res     = Input(Vec(cols, UInt(config.accWidth.W)))  // output of Systolic Array
    val in_en      = Input(Bool())                              // High when S.A. producing results

    val out_stream = Decoupled(UInt(config.accWidth.W))         // Interface to the Memory Bus (AXI-Stream style)
  })

  // BANK OF FIFOs : These capture the "staggered" results from the mesh.
  // TODO : Dynamic depth to adapt to RAM speed (atm 16)
  val queues = Seq.fill(cols)(Module(new Queue(UInt(config.accWidth.W), entries = 16)))

  for (i <- 0 until cols) {
    queues(i).io.enq.bits  := io.in_res(i)
    queues(i).io.enq.valid := io.in_en
    // Note: In a real design, you'd check queues(i).io.enq.ready
    // to prevent overflow, but here we assume the RAM is fast enough.
  }

  //ROUND-ROBIN DRAINER (The FSM) : picks results one by one: Col0, then Col1, then Col2...
  val fifoDeqs = VecInit(queues.map(_.io.deq))
  val colIdx = RegInit(0.U(log2Ceil(cols).W))

  // 1. Connect the output stream to the currently selected FIFO
  io.out_stream.valid := fifoDeqs(colIdx).valid
  io.out_stream.bits  := fifoDeqs(colIdx).bits

  // 2. Drive the 'ready' signals for ALL FIFOs
  for (i <- 0 until cols) {
    // A FIFO is ready ONLY if it's the one currently selected AND the output accepts data
    fifoDeqs(i).ready := (colIdx === i.U) && io.out_stream.ready
  }

  // 3. Increment the index when a successful handshake occurs
  when(io.out_stream.fire) { // .fire is shorthand for (valid && ready)
    colIdx := Mux(colIdx === (cols - 1).U, 0.U, colIdx + 1.U)
  }
}
