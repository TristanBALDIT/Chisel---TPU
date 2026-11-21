import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SystolicArrayTester extends AnyFlatSpec with ChiselScalatestTester {
  val cfg = Int8Config()

  "SystolicArray" should "multiply 2x2 correctly and assert valid signals" in {
    test(new SystolicArray(2, 2, cfg)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      // --- MATRICES DEFINITION (C = A * W, all row-major) ---
      // Matrix A (M=2 x K=2)     Matrix W (K=2 x N=2)     Result C = A * W (M=2 x N=2)
      // | 10  30 |          *    | 1  2 |              =  | 100  140 |
      // | 20  40 |               | 3  4 |                 | 140  200 |
      //
      // MATH CHECK:
      // Row 0 of C: (10*1 + 30*3) = 100,  (10*2 + 30*4) = 140
      // Row 1 of C: (20*1 + 40*3) = 140,  (20*2 + 40*4) = 200

      // Initialization
      dut.io.load.poke(false.B)
      dut.io.stall.poke(false.B)
      dut.io.in_v(0).poke(0.U)
      dut.io.in_v(1).poke(0.U)
      dut.io.in_v_valid(0).poke(false.B)
      dut.io.in_v_valid(1).poke(false.B)
      dut.io.in_w(0).poke(0.U)
      dut.io.in_w(1).poke(0.U)
      dut.io.in_w_valid(0).poke(false.B)
      dut.io.in_w_valid(1).poke(false.B)
      dut.clock.step()

      // --- PHASE 1 : WEIGHTS (Vertical Shift Register, Row-Major) ---
      // Row 0 of W (PE Row 0): [1, 2]
      // Row 1 of W (PE Row 1): [3, 4]
      dut.io.load.poke(true.B)

      // Cycle 0: bottom row of weights (3 and 4)
      dut.io.in_w(0).poke(3.U)
      dut.io.in_w(1).poke(4.U)
      dut.io.in_w_valid(0).poke(true.B)
      dut.io.in_w_valid(1).poke(true.B)
      dut.clock.step()

      // Cycle 1: top row of weights (1 and 2)
      dut.io.in_w(0).poke(1.U)
      dut.io.in_w(1).poke(2.U)
      dut.io.in_w_valid(0).poke(true.B)
      dut.io.in_w_valid(1).poke(true.B)
      dut.clock.step()

      dut.io.load.poke(false.B)
      dut.io.in_w_valid(0).poke(false.B)
      dut.io.in_w_valid(1).poke(false.B)
      dut.clock.step()

      // --- PHASE 2: COMPUTE (Streaming Rows of A with systolic skew) ---
      // Matrix A (row-major) = | 10  30 |  <- Row 0 (m=0)
      //                        | 20  40 |  <- Row 1 (m=1)

      // T=0: A(0,0)=10 enters Row 0 (valid), Row 1 is idle/invalid
      dut.io.in_v(0).poke(10.U)
      dut.io.in_v_valid(0).poke(true.B)
      dut.io.in_v(1).poke(0.U)
      dut.io.in_v_valid(1).poke(false.B)
      dut.clock.step()

      // T=1: A(1,0)=20 enters Row 0 | A(0,1)=30 enters Row 1 (skewed)
      dut.io.in_v(0).poke(20.U)
      dut.io.in_v_valid(0).poke(true.B)
      dut.io.in_v(1).poke(30.U)
      dut.io.in_v_valid(1).poke(true.B)
      dut.clock.step()

      // T=2: Row 0 done | A(1,1)=40 enters Row 1
      dut.io.in_v(0).poke(0.U)
      dut.io.in_v_valid(0).poke(false.B)
      dut.io.in_v(1).poke(40.U)
      dut.io.in_v_valid(1).poke(true.B)

      // Col 0 output emerges: (1*10 + 3*30) = 100
      dut.io.out_res(0).expect(100.U)
      dut.io.out_valid(0).expect(true.B)
      dut.io.out_valid(1).expect(false.B) // Col 1 not ready yet (skewed)
      dut.clock.step()

      // T=3: No more inputs
      dut.io.in_v(1).poke(0.U)
      dut.io.in_v_valid(1).poke(false.B)

      // Both columns produce outputs:
      // Col 0: (1*20 + 3*40) = 140
      // Col 1: (2*10 + 4*30) = 140
      dut.io.out_res(0).expect(140.U)
      dut.io.out_valid(0).expect(true.B)
      dut.io.out_res(1).expect(140.U)
      dut.io.out_valid(1).expect(true.B)
      dut.clock.step()

      // T=4: Last element for Col 1: (2*20 + 4*40) = 200
      dut.io.out_res(1).expect(200.U)
      dut.io.out_valid(1).expect(true.B)
      dut.io.out_valid(0).expect(false.B) // Col 0 is drained
      dut.clock.step()

      // T=5: All drained
      dut.io.out_valid(0).expect(false.B)
      dut.io.out_valid(1).expect(false.B)
    }
  }

  it should "support sub-matrix multiplication by bypassing inactive PEs" in {
    test(new SystolicArray(2, 2, cfg)) { dut =>
      dut.io.stall.poke(false.B)
      dut.io.load.poke(false.B)
      dut.io.in_v_valid(0).poke(false.B)
      dut.io.in_v_valid(1).poke(false.B)
      dut.io.in_w_valid(0).poke(false.B)
      dut.io.in_w_valid(1).poke(false.B)
      dut.clock.step()

      // Load 1x2 sub-matrix weight W = [5, 7] (K=1 row, N=2 cols, row-major):
      // PE Row 0 has Row 0 of W = [5, 7] (valid = true)
      // PE Row 1 is inactive (valid = false)
      dut.io.load.poke(true.B)
      // Cycle 0: bottom row (Row 1) - marked invalid
      dut.io.in_w(0).poke(0.U)
      dut.io.in_w(1).poke(0.U)
      dut.io.in_w_valid(0).poke(false.B)
      dut.io.in_w_valid(1).poke(false.B)
      dut.clock.step()

      // Cycle 1: top row (Row 0) - [5, 7] valid
      dut.io.in_w(0).poke(5.U)
      dut.io.in_w(1).poke(7.U)
      dut.io.in_w_valid(0).poke(true.B)
      dut.io.in_w_valid(1).poke(true.B)
      dut.clock.step()

      dut.io.load.poke(false.B)
      dut.io.in_w_valid(0).poke(false.B)
      dut.io.in_w_valid(1).poke(false.B)
      dut.clock.step()

      // Stream A = [10] (M=1 x K=1) on Row 0 (Row 1 is inactive)
      // C = A * W = [10] * [5, 7] = [50, 70]
      dut.io.in_v(0).poke(10.U)
      dut.io.in_v_valid(0).poke(true.B)
      dut.io.in_v(1).poke(0.U)
      dut.io.in_v_valid(1).poke(false.B)
      dut.clock.step()

      dut.io.in_v(0).poke(0.U)
      dut.io.in_v_valid(0).poke(false.B)

      // Cycle delay: Row 0 computes at T=0, passes through Row 1 (bypass) at T=1, emerges at T=2 for Col 0
      dut.clock.step()

      // Col 0 output emerges: 50
      dut.io.out_res(0).expect(50.U)
      dut.io.out_valid(0).expect(true.B)
      dut.io.out_valid(1).expect(false.B)
      dut.clock.step()

      // Col 1 output emerges: 70
      dut.io.out_res(1).expect(70.U)
      dut.io.out_valid(1).expect(true.B)
      dut.io.out_valid(0).expect(false.B)
      dut.clock.step()

      dut.io.out_valid(0).expect(false.B)
      dut.io.out_valid(1).expect(false.B)
    }
  }

  it should "freeze array state when stall is asserted" in {
    test(new SystolicArray(2, 2, cfg)) { dut =>
      dut.io.stall.poke(false.B)
      dut.io.load.poke(false.B)
      dut.io.in_v_valid(0).poke(false.B)
      dut.io.in_v_valid(1).poke(false.B)
      dut.io.in_w_valid(0).poke(false.B)
      dut.io.in_w_valid(1).poke(false.B)
      dut.clock.step()

      // Load weights: Row 0 = [1, 2], Row 1 = [3, 4]
      dut.io.load.poke(true.B)
      dut.io.in_w(0).poke(3.U); dut.io.in_w(1).poke(4.U)
      dut.io.in_w_valid(0).poke(true.B); dut.io.in_w_valid(1).poke(true.B)
      dut.clock.step()

      dut.io.in_w(0).poke(1.U); dut.io.in_w(1).poke(2.U)
      dut.io.in_w_valid(0).poke(true.B); dut.io.in_w_valid(1).poke(true.B)
      dut.clock.step()

      dut.io.load.poke(false.B)
      dut.io.in_w_valid(0).poke(false.B); dut.io.in_w_valid(1).poke(false.B)
      dut.clock.step()

      // Feed input 10 on Row 0
      dut.io.in_v(0).poke(10.U); dut.io.in_v_valid(0).poke(true.B)
      dut.clock.step()

      // Now freeze the array with stall
      dut.io.stall.poke(true.B)
      dut.io.in_v(0).poke(99.U) // Changed input while stalled (should be ignored)
      dut.clock.step(3)

      // Unfreeze
      dut.io.stall.poke(false.B)
      dut.io.in_v(0).poke(0.U); dut.io.in_v_valid(0).poke(false.B)
      dut.clock.step()

      // The in-flight calculation should complete without corruption
      dut.io.out_res(0).expect(10.U) // Row 0 (10*1) passed down Row 1 (+ 0)
      dut.io.out_valid(0).expect(true.B)
    }
  }
}
