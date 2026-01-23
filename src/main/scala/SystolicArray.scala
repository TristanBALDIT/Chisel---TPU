
import chisel3._
import chisel3.util._

class SystolicArray(val rows: Int, val cols: Int, val w: Int) extends Module {
  val io = IO(new Bundle {
    val in_a = Input(Vec(rows, UInt(w.W))) // Entrées par la gauche
    val in_b = Input(Vec(cols, UInt(w.W))) // Entrées par le haut
    val ctrl_en = Input(Bool())
    val results = Output(Vec(rows, Vec(cols, UInt((2*w).W))))
  })

  // Création de la grille de nœuds
  val mesh = Seq.fill(rows, cols)(Module(new Node(w)))

  for (r <- 0 until rows) {
    for (c <- 0 until cols) {
      val node = mesh(r)(c)
      node.io.ctrl_en := io.ctrl_en

      // Horizontal DATA connexion
      if (c == 0) node.io.in_a := io.in_a(r)
      else node.io.in_a := mesh(r)(c - 1).io.out_a

      // Vertical DATA connexion
      if (r == 0) node.io.in_b := io.in_b(c)
      else node.io.in_b := mesh(r - 1)(c).io.out_b

      // Output
      io.results(r)(c) := node.io.res
    }
  }
}
