package tpu.input

import tpu.config._
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ActivationControllerTester extends AnyFlatSpec with ChiselScalatestTester {
  val cfg = Int8Config(rows = 2, cols = 2, weightBramDepth = 16, actBramDepth = 16)

  "ActivationController" should "fetch from memory and stream skewed activations with valid tokens" in {
    test(new ActivationController(cfg)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Mock Activation Memory (row-major for C = A * W, where A is M=2 x K=2):
      // A = | 10  30 | -> Addr 0 (Row 0 of A, m=0): [10, 30]
      //     | 20  40 | -> Addr 1 (Row 1 of A, m=1): [20, 40]
      val mem = Map(
        0 -> Seq(10, 30),
        1 -> Seq(20, 40)
      )

      dut.io.stall.poke(false.B)
      dut.io.start.poke(false.B)
      dut.io.baseAddr.poke(0.U)
      dut.io.matrixM.poke(2.U)
      dut.io.matrixK.poke(2.U)
      dut.io.mem_data(0).poke(0.U)
      dut.io.mem_data(1).poke(0.U)
      dut.clock.step()

      // Pulse start
      dut.io.start.poke(true.B)
      dut.clock.step()
      dut.io.start.poke(false.B)

      // Cycle 1: State is sFetch, issuing read for address 0
      dut.io.busy.expect(true.B)
      dut.io.mem_en.expect(true.B)
      dut.io.mem_addr.expect(0.U)
      dut.clock.step()

      // Cycle 2: Memory returns data for address 0 ([10, 30])
      // Controller issues read for address 1
      dut.io.mem_addr.expect(1.U)
      dut.io.mem_en.expect(true.B)
      dut.io.mem_data(0).poke(mem(0)(0).U)
      dut.io.mem_data(1).poke(mem(0)(1).U)

      // SkewBuffer Row 0 (0 delay) outputs 10 immediately
      dut.io.out_v(0).expect(10.U)
      dut.io.out_v_valid(0).expect(true.B)
      dut.io.out_v_valid(1).expect(false.B) // Row 1 delayed by 1 cycle
      dut.clock.step()

      // Cycle 3: Memory returns data for address 1 ([20, 40])
      // Row 0 outputs Vector 1 (20) with 0 delay
      // Row 1 outputs Vector 0 (30) with 1 cycle delay
      dut.io.mem_data(0).poke(mem(1)(0).U)
      dut.io.mem_data(1).poke(mem(1)(1).U)

      dut.io.out_v(0).expect(20.U)
      dut.io.out_v_valid(0).expect(true.B)
      dut.io.out_v(1).expect(30.U)
      dut.io.out_v_valid(1).expect(true.B)
      dut.clock.step()

      // Cycle 4: Memory reads complete. Row 0 has no more valid data.
      // Row 1 outputs Vector 1 (40) with 1 cycle delay
      dut.io.mem_data(0).poke(0.U)
      dut.io.mem_data(1).poke(0.U)

      dut.io.out_v_valid(0).expect(false.B)
      dut.io.out_v(1).expect(40.U)
      dut.io.out_v_valid(1).expect(true.B)
      dut.clock.step()

      // Cycle 5: Skew pipeline fully drained, done is asserted
      dut.io.out_v_valid(0).expect(false.B)
      dut.io.out_v_valid(1).expect(false.B)
      dut.io.done.expect(true.B)
      dut.clock.step()

      // Cycle 6: Back to idle
      dut.io.busy.expect(false.B)
      dut.io.done.expect(false.B)
    }
  }

  it should "automatically mask inactive rows for sub-matrices" in {
    test(new ActivationController(cfg)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // Sub-matrix: K = 1 (only Row 0 active, Row 1 inactive)
      val mem = Map(
        0 -> Seq(55, 99),
        1 -> Seq(66, 99)
      )

      dut.io.stall.poke(false.B)
      dut.io.start.poke(false.B)
      dut.io.baseAddr.poke(0.U)
      dut.io.matrixM.poke(2.U)
      dut.io.matrixK.poke(1.U) // <--- K = 1: only Row 0 active!
      dut.io.mem_data(0).poke(0.U)
      dut.io.mem_data(1).poke(0.U)
      dut.clock.step()

      // Pulse start
      dut.io.start.poke(true.B)
      dut.clock.step()
      dut.io.start.poke(false.B)

      // Cycle 1: Request address 0
      dut.io.mem_en.expect(true.B)
      dut.io.mem_addr.expect(0.U)
      dut.clock.step()

      // Cycle 2: Memory returns Vector 0 ([55, 99])
      dut.io.mem_data(0).poke(mem(0)(0).U)
      dut.io.mem_data(1).poke(mem(0)(1).U)

      // Row 0 is valid (55), Row 1 is masked (valid = false)
      dut.io.out_v(0).expect(55.U)
      dut.io.out_v_valid(0).expect(true.B)
      dut.io.out_v_valid(1).expect(false.B)
      dut.clock.step()

      // Cycle 3: Memory returns Vector 1 ([66, 99])
      dut.io.mem_data(0).poke(mem(1)(0).U)
      dut.io.mem_data(1).poke(mem(1)(1).U)

      // Row 0 is valid (66), Row 1 remains masked (valid = false)
      dut.io.out_v(0).expect(66.U)
      dut.io.out_v_valid(0).expect(true.B)
      dut.io.out_v_valid(1).expect(false.B) // Row 1 masked!
      dut.clock.step()

      // Cycle 4: Draining
      dut.io.mem_data(0).poke(0.U)
      dut.io.mem_data(1).poke(0.U)
      dut.io.out_v_valid(0).expect(false.B)
      dut.io.out_v_valid(1).expect(false.B) // Row 1 remains false!
      dut.clock.step()

      // Cycle 5: Done
      dut.io.done.expect(true.B)
    }
  }

  it should "freeze execution and SkewBuffer when stall is asserted" in {
    test(new ActivationController(cfg)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.io.stall.poke(false.B)
      dut.io.start.poke(false.B)
      dut.io.baseAddr.poke(0.U)
      dut.io.matrixM.poke(2.U)
      dut.io.matrixK.poke(2.U)
      dut.io.mem_data(0).poke(0.U)
      dut.io.mem_data(1).poke(0.U)
      dut.clock.step()

      dut.io.start.poke(true.B)
      dut.clock.step()
      dut.io.start.poke(false.B)

      // Cycle 1: Request addr 0
      dut.io.mem_en.expect(true.B)
      dut.io.mem_addr.expect(0.U)
      dut.clock.step()

      // Cycle 2: Feed addr 0 ([10, 30]). Row 0 (0 delay) outputs 10 combinatorially.
      // Row 1's shift register is about to latch 30 on the upcoming clock edge.
      dut.io.mem_data(0).poke(10.U)
      dut.io.mem_data(1).poke(30.U)
      dut.io.out_v(0).expect(10.U)
      dut.io.out_v_valid(0).expect(true.B)
      dut.io.out_v_valid(1).expect(false.B)
      // Step FIRST so Row 1 latches 30 into its delay register, THEN assert stall.
      dut.clock.step()

      // Cycle 3: Row 1 has now latched 30. Assert stall NOW to freeze it there.
      // Row 1 outputs 30 combinatorially (SkewBuffer out = dataRegs(0) = 30).
      dut.io.stall.poke(true.B)
      dut.io.mem_data(0).poke(0.U)
      dut.io.mem_data(1).poke(0.U)
      dut.io.busy.expect(true.B)
      dut.io.out_v(1).expect(30.U)
      dut.io.out_v_valid(1).expect(true.B)

      // Hold stall for 3 cycles: all registers stay frozen at 30
      dut.clock.step(3)
      dut.io.out_v(1).expect(30.U)
      dut.io.out_v_valid(1).expect(true.B)
      dut.io.busy.expect(true.B)

      // Unfreeze stall: controller resumes fetching addr 1
      dut.io.stall.poke(false.B)
      dut.io.mem_data(0).poke(20.U)
      dut.io.mem_data(1).poke(40.U)

      // Row 0 outputs Vector 1 (20), Row 1 is still holding 30
      dut.io.out_v(0).expect(20.U)
      dut.io.out_v_valid(0).expect(true.B)
      dut.io.out_v(1).expect(30.U)
      dut.io.out_v_valid(1).expect(true.B)
      dut.clock.step()

      // Row 0 done, Row 1 latches and emits 40
      dut.io.mem_data(0).poke(0.U)
      dut.io.mem_data(1).poke(0.U)
      dut.io.out_v_valid(0).expect(false.B)
      dut.io.out_v(1).expect(40.U)
      dut.io.out_v_valid(1).expect(true.B)
      dut.clock.step()

      // Pipeline drained, done asserted
      dut.io.done.expect(true.B)
    }
  }
}
