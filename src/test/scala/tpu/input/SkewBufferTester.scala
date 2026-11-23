package tpu.input

import tpu.config._
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SkewBufferTester extends AnyFlatSpec with ChiselScalatestTester {
  val cfg = Int8Config()

  "SkewBuffer" should "stagger inputs and valid tokens correctly" in {
    test(new SkewBuffer(3, cfg)) { dut =>
      dut.io.stall.poke(false.B)

      // Cycle 0: Provide [10, 20, 30] all valid at the same time
      dut.io.in(0).poke(10.U); dut.io.in_valid(0).poke(true.B)
      dut.io.in(1).poke(20.U); dut.io.in_valid(1).poke(true.B)
      dut.io.in(2).poke(30.U); dut.io.in_valid(2).poke(true.B)

      // Row 0 has 0 delay -> available immediately
      dut.io.out(0).expect(10.U); dut.io.out_valid(0).expect(true.B)
      dut.io.out_valid(1).expect(false.B)
      dut.io.out_valid(2).expect(false.B)

      dut.clock.step()

      // Cycle 1: Stop input
      dut.io.in(0).poke(0.U); dut.io.in_valid(0).poke(false.B)
      dut.io.in(1).poke(0.U); dut.io.in_valid(1).poke(false.B)
      dut.io.in(2).poke(0.U); dut.io.in_valid(2).poke(false.B)

      // Row 1 has 1 cycle delay -> available at cycle 1
      dut.io.out_valid(0).expect(false.B)
      dut.io.out(1).expect(20.U); dut.io.out_valid(1).expect(true.B)
      dut.io.out_valid(2).expect(false.B)

      dut.clock.step()

      // Cycle 2: Row 2 has 2 cycles delay -> available at cycle 2
      dut.io.out_valid(0).expect(false.B)
      dut.io.out_valid(1).expect(false.B)
      dut.io.out(2).expect(30.U); dut.io.out_valid(2).expect(true.B)

      dut.clock.step()

      // Cycle 3: All drained
      dut.io.out_valid(0).expect(false.B)
      dut.io.out_valid(1).expect(false.B)
      dut.io.out_valid(2).expect(false.B)
    }
  }

  it should "freeze internal registers when stall is asserted" in {
    test(new SkewBuffer(3, cfg)) { dut =>
      dut.io.stall.poke(false.B)

      // Input data
      dut.io.in(0).poke(1.U); dut.io.in_valid(0).poke(true.B)
      dut.io.in(1).poke(2.U); dut.io.in_valid(1).poke(true.B)
      dut.io.in(2).poke(3.U); dut.io.in_valid(2).poke(true.B)
      dut.clock.step()

      // Assert stall
      dut.io.stall.poke(true.B)
      dut.io.in(0).poke(99.U); dut.io.in_valid(0).poke(false.B)
      dut.io.in(1).poke(99.U); dut.io.in_valid(1).poke(false.B)
      dut.io.in(2).poke(99.U); dut.io.in_valid(2).poke(false.B)
      dut.clock.step(3)

      // Row 1 should still hold 2 (delayed by 1 cycle, frozen for 3 cycles)
      dut.io.out(1).expect(2.U)
      dut.io.out_valid(1).expect(true.B)

      // Unfreeze
      dut.io.stall.poke(false.B)
      dut.clock.step()

      // Row 2 now emerges with 3
      dut.io.out(2).expect(3.U)
      dut.io.out_valid(2).expect(true.B)
    }
  }
}
