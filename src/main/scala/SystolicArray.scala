import chisel3._
import chisel3.util._

class SystolicArray(val rows: Int, val cols: Int, config: TpuConfig) extends Module {
  val io = IO(new Bundle {
    // Control
    val load  = Input(Bool())
    val stall = Input(Bool())

    // Inputs: Vector of activations (left edge)
    val in_v       = Input(Vec(rows, UInt(config.dataWidth.W)))
    val in_v_valid = Input(Vec(rows, Bool()))

    // Inputs: Vector of weights (top edge)
    val in_w       = Input(Vec(cols, UInt(config.dataWidth.W)))
    val in_w_valid = Input(Vec(cols, Bool()))

    // Outputs: Bottom edge of the mesh (partial sums and valids)
    val out_res   = Output(Vec(cols, UInt(config.accWidth.W)))
    val out_valid = Output(Vec(cols, Bool()))
  })

  // 1. Instantiate the 2D grid of PEs
  val mesh = Seq.fill(rows, cols)(Module(new PE(config)))

  // 2. Connect the nodes
  for (r <- 0 until rows) {
    for (c <- 0 until cols) {
      val node = mesh(r)(c)

      // Global control signals
      node.io.load  := io.load
      node.io.stall := io.stall

      // Horizontal flow (Activations + Valid token)
      if (c == 0) {
        node.io.in_v       := io.in_v(r)
        node.io.in_v_valid := io.in_v_valid(r)
      } else {
        node.io.in_v       := mesh(r)(c - 1).io.out_v
        node.io.in_v_valid := mesh(r)(c - 1).io.out_v_valid
      }

      // Vertical flow (Weights, Weight Valid token, and Accumulators)
      if (r == 0) {
        node.io.in_w         := io.in_w(c)
        node.io.in_w_valid   := io.in_w_valid(c)
        node.io.in_acc       := 0.U
        node.io.in_acc_valid := false.B
      } else {
        node.io.in_w         := mesh(r - 1)(c).io.out_w
        node.io.in_w_valid   := mesh(r - 1)(c).io.out_w_valid
        node.io.in_acc       := mesh(r - 1)(c).io.out_acc
        node.io.in_acc_valid := mesh(r - 1)(c).io.out_acc_valid
      }

      // 3. Connect final outputs from the bottom row
      if (r == rows - 1) {
        io.out_res(c)   := node.io.out_acc
        io.out_valid(c) := node.io.out_acc_valid
      }
    }
  }
}
