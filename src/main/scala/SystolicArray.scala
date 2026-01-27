
import chisel3._
import chisel3.util._

class SystolicArray(val rows: Int, val cols: Int, config: TpuConfig) extends Module {
  val io = IO(new Bundle {
    val load    = Input(Bool())
    val en      = Input(Bool())

    // Inputs: Vector of activations (left) and Weights/Partial Sums (top)
    val in_v    = Input(Vec(rows, UInt(config.dataWidth.W)))
    val in_w    = Input(Vec(cols, UInt(config.accWidth.W)))

    // Outputs: Bottom of the mesh
    val out_res = Output(Vec(cols, UInt(config.accWidth.W)))
  })

  // 1. Instantiate the grid
  val mesh = Seq.fill(rows, cols)(Module(new PE(config)))

  // 2. Connect the nodes
  for (r <- 0 until rows) {
    for (c <- 0 until cols) {
      val node = mesh(r)(c)

      // Global control signals
      node.io.load := io.load
      node.io.en   := io.en

      // Horizontal flow (Activations)
      if (c == 0) {
        node.io.in_v := io.in_v(r)
      } else {
        node.io.in_v := mesh(r)(c - 1).io.out_v
      }

      // Vertical flow (Weights or Partial Sums)
      if (r == 0) {
        node.io.in_w := io.in_w(c)
        node.io.in_acc := 0.U
      } else {
        node.io.in_acc := mesh(r - 1)(c).io.out_acc
        node.io.in_w := mesh(r - 1)(c).io.out_w
      }

      // 3. Connect final outputs (from the last row)
      if (r == rows - 1) {
        io.out_res(c) := node.io.out_acc
      }
    }
  }
}
