import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SramOutputSystemTester extends AnyFlatSpec with ChiselScalatestTester {
  val cfg = Int8Config()

  "SramOutputSystem" should "absorb skewed outputs and emit parallel rows" in {
    test(new SramOutputSystem(2, 2, depth = 8, cfg)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.io.out_row.ready.poke(true.B)
      dut.io.out_row.valid.expect(false.B)
      dut.io.stall.expect(false.B)

      // Cycle 0: Col 0 outputs Row 0, Col 0
      dut.io.in_res(0).poke(100.U)
      dut.io.in_en(0).poke(true.B)
      dut.io.in_en(1).poke(false.B)
      dut.clock.step()

      // Cycle 1: Col 0 outputs Row 1, Col 0; Col 1 outputs Row 0, Col 1
      dut.io.in_res(0).poke(140.U)
      dut.io.in_res(1).poke(101.U) // Row 0, Col 1
      dut.io.in_en(0).poke(true.B)
      dut.io.in_en(1).poke(true.B)
      dut.clock.step()

      // Cycle 2: Col 1 outputs Row 1, Col 1
      dut.io.in_res(1).poke(201.U) // Row 1, Col 1
      dut.io.in_en(0).poke(false.B)
      dut.io.in_en(1).poke(true.B)

      // SRAM read latency is 1 cycle; skid buffer delivers data
      dut.clock.step()

      // No more inputs
      dut.io.in_en(0).poke(false.B)
      dut.io.in_en(1).poke(false.B)

      // Verify Row 0 = [100, 101]
      dut.io.out_row.valid.expect(true.B)
      dut.io.out_row.bits(0).expect(100.U)
      dut.io.out_row.bits(1).expect(101.U)
      dut.clock.step()

      // Verify Row 1 = [140, 201]
      dut.io.out_row.valid.expect(true.B)
      dut.io.out_row.bits(0).expect(140.U)
      dut.io.out_row.bits(1).expect(201.U)
      dut.clock.step()

      // Drained completely
      dut.io.out_row.valid.expect(false.B)
    }
  }

  it should "stall cleanly when backpressured by downstream" in {
    test(new SramOutputSystem(2, 2, depth = 8, cfg)) { dut =>
      dut.io.out_row.ready.poke(false.B) // Downstream stalls immediately

      // Send Row 0 and Row 1 with skew
      // Cycle 0: Col 0
      dut.io.in_res(0).poke(10.U)
      dut.io.in_en(0).poke(true.B)
      dut.io.in_en(1).poke(false.B)
      dut.clock.step()

      // Cycle 1: Col 0 & Col 1
      dut.io.in_res(0).poke(20.U)
      dut.io.in_res(1).poke(11.U)
      dut.io.in_en(0).poke(true.B)
      dut.io.in_en(1).poke(true.B)
      dut.clock.step()

      // Cycle 2: Col 1
      dut.io.in_res(1).poke(21.U)
      dut.io.in_en(0).poke(false.B)
      dut.io.in_en(1).poke(true.B)
      dut.clock.step()

      dut.io.in_en(0).poke(false.B)
      dut.io.in_en(1).poke(false.B)
      dut.clock.step(2) // Wait while backpressured

      // Row 0 must be held valid and waiting at output
      dut.io.out_row.valid.expect(true.B)
      dut.io.out_row.bits(0).expect(10.U)
      dut.io.out_row.bits(1).expect(11.U)

      // Open ready
      dut.io.out_row.ready.poke(true.B)
      dut.clock.step()

      // Row 1 should follow immediately
      dut.io.out_row.valid.expect(true.B)
      dut.io.out_row.bits(0).expect(20.U)
      dut.io.out_row.bits(1).expect(21.U)
      dut.clock.step()

      // Now empty
      dut.io.out_row.valid.expect(false.B)
    }
  }
}
