package tpu.core

import tpu.config._
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class PETester extends AnyFlatSpec with ChiselScalatestTester {
  val cfg = Int8Config()

  "PE" should "load a weight and compute a MAC operation with a 1-cycle delay" in {
    test(new PE(cfg)) { dut =>
      dut.io.stall.poke(false.B)

      // Load the Weight (W = 5, valid = true)
      dut.io.load.poke(true.B)
      dut.io.in_w.poke(5.U)
      dut.io.in_w_valid.poke(true.B)
      dut.io.in_v.poke(0.U)
      dut.io.in_v_valid.poke(false.B)
      dut.io.in_acc.poke(0.U)
      dut.io.in_acc_valid.poke(false.B)
      dut.clock.step()

      dut.io.load.poke(false.B)

      // Feed Activation (V = 10, valid = true) and Partial Sum (Acc = 3, in_acc_valid = false)
      dut.io.in_v.poke(10.U)
      dut.io.in_v_valid.poke(true.B)
      dut.io.in_acc.poke(3.U)
      dut.io.in_acc_valid.poke(false.B)
      dut.clock.step()

      // The MAC result takes one cycle due to internal register
      // Verify MAC: 10 * 5 + 3 = 53
      dut.io.out_acc.expect(53.U)
      dut.io.out_acc_valid.expect(true.B)

      // Verify Activation passes through unchanged
      dut.io.out_v.expect(10.U)
      dut.io.out_v_valid.expect(true.B)
    }
  }

  it should "bypass computation when activation is not valid" in {
    test(new PE(cfg)) { dut =>
      dut.io.stall.poke(false.B)

      // Load Weight = 5
      dut.io.load.poke(true.B)
      dut.io.in_w.poke(5.U)
      dut.io.in_w_valid.poke(true.B)
      dut.io.in_v_valid.poke(false.B)
      dut.io.in_acc_valid.poke(false.B)
      dut.clock.step()
      dut.io.load.poke(false.B)

      // Feed invalid activation (in_v_valid = false) with incoming partial sum 42
      dut.io.in_v.poke(10.U)
      dut.io.in_v_valid.poke(false.B)
      dut.io.in_acc.poke(42.U)
      dut.io.in_acc_valid.poke(true.B)
      dut.clock.step()

      // Should bypass: pass 42 through without adding (10 * 5)
      dut.io.out_acc.expect(42.U)
      dut.io.out_acc_valid.expect(true.B)
      dut.io.out_v_valid.expect(false.B)
    }
  }

  it should "bypass computation when weight is not valid" in {
    test(new PE(cfg)) { dut =>
      dut.io.stall.poke(false.B)

      // Load Weight with in_w_valid = false
      dut.io.load.poke(true.B)
      dut.io.in_w.poke(5.U)
      dut.io.in_w_valid.poke(false.B)
      dut.io.in_v_valid.poke(false.B)
      dut.io.in_acc_valid.poke(false.B)
      dut.clock.step()
      dut.io.load.poke(false.B)

      // Feed valid activation 10 and incoming partial sum 7
      dut.io.in_v.poke(10.U)
      dut.io.in_v_valid.poke(true.B)
      dut.io.in_acc.poke(7.U)
      dut.io.in_acc_valid.poke(true.B)
      dut.clock.step()

      // Should bypass: pass 7 through without multiplying
      dut.io.out_acc.expect(7.U)
      dut.io.out_acc_valid.expect(true.B)
    }
  }

  it should "freeze all registers when stall is asserted" in {
    test(new PE(cfg)) { dut =>
      dut.io.stall.poke(false.B)

      // Load weight = 4
      dut.io.load.poke(true.B)
      dut.io.in_w.poke(4.U)
      dut.io.in_w_valid.poke(true.B)
      dut.io.in_v_valid.poke(false.B)
      dut.io.in_acc_valid.poke(false.B)
      dut.clock.step()
      dut.io.load.poke(false.B)

      // Step 1: compute 2 * 4 + 1 = 9
      dut.io.in_v.poke(2.U)
      dut.io.in_v_valid.poke(true.B)
      dut.io.in_acc.poke(1.U)
      dut.io.in_acc_valid.poke(true.B)
      dut.clock.step()

      dut.io.out_acc.expect(9.U)
      dut.io.out_v.expect(2.U)

      // Step 2: assert stall, change inputs
      dut.io.stall.poke(true.B)
      dut.io.in_v.poke(99.U)
      dut.io.in_acc.poke(999.U)
      dut.clock.step()

      // Values should remain frozen at 9 and 2
      dut.io.out_acc.expect(9.U)
      dut.io.out_v.expect(2.U)
      dut.io.out_acc_valid.expect(true.B)
    }
  }
}
