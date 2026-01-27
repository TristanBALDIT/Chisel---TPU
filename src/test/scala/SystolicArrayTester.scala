import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SystolicArrayTester extends AnyFlatSpec with ChiselScalatestTester {
  // On utilise une config Int8 pour le test
  val cfg = Int8Config()

  "SystolicArray" should "multiply 2x2 correctly" in {
    test(new SystolicArray(2, 2, cfg)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      // --- MATRICES DEFINITION ---
      // Matrix W (weights)       Matrix A (Activation)
      // | 1  3 |            *    | 10  20 |
      // | 2  4 |                 | 30  40 |

      // MATH CHECK:
      // Row0: (1*10 + 3*30) = 100,  (1*20 + 3*40) = 140
      // Row1: (2*10 + 4*30) = 140, (2*20 + 4*40) = 200

      // Initialisation
      dut.io.load.poke(false.B)
      dut.io.en.poke(false.B)
      dut.clock.step()

      // --- PHASE 1 : WEIGHTS (Shift Register) ---
      // W Matrix :   | 1  3 |
      //              | 2  4 |

      // Matrix transposition to fit the systolic array
      // Column 0 : 1 then 3 | Column 1 : 2 then 4 (inversion ligne/column)

      dut.io.load.poke(true.B)

      // Cycle 0: bottom lines of systolic array
      dut.io.in_w(0).poke(3.U)
      dut.io.in_w(1).poke(4.U)
      dut.clock.step()

      // Cycle 1: top lines of systolic array (1 et 2)
      dut.io.in_w(0).poke(1.U)
      dut.io.in_w(1).poke(2.U)
      dut.clock.step()

      dut.io.load.poke(false.B)
      dut.clock.step()
      println("--- Weights Matrix W Loaded ---")

      // --- PHASE 2: COMPUTE (Streaming A) ---
      // Matrix A = | 10  20 |
      //            | 30  40 |

      dut.io.en.poke(true.B)

      // T=0: A(0,0) enters Row 0
      dut.io.in_v(0).poke(10.U)
      dut.io.in_v(1).poke(0.U)
      dut.clock.step()

      // T=1: A(1,0) enters Row 0 | A(0,1) enters Row 1 (Skewed)
      dut.io.in_v(0).poke(20.U)
      dut.io.in_v(1).poke(30.U)
      dut.clock.step()

      // T=2: A(1,1) enters Row 1
      dut.io.in_v(0).poke(0.U)
      dut.io.in_v(1).poke(40.U)

      println(s"T=2 | W_row0 * A_col0: ${dut.io.out_res(0).peek().litValue}")
      dut.clock.step()

      // T=3: Result for W_row0 * A_col1 &&  W_row1 * A_col0
      dut.io.in_v(1).poke(0.U)

      dut.io.out_res(0).expect(140.U)
      dut.io.out_res(1).expect(140.U)
      println(s"T=3 | W_row0 * A_col1: ${dut.io.out_res(0).peek().litValue}")
      println(s"T=3 | W_row1 * A_col0: ${dut.io.out_res(1).peek().litValue}")

      dut.clock.step(1)

      // T=4: Result for W_row1 * A_col1
      dut.io.out_res(1).expect(200.U)
      println(s"T=4 | W_row1 * A_col1: ${dut.io.out_res(1).peek().litValue}")

      // Results expected :
      //    | 100  140 |
      //    | 140  200 |
    }
  }
}
