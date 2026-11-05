import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ReverseSkewOutputSystemTester extends AnyFlatSpec with ChiselScalatestTester {
  val cfg = Int8Config()

  "ReverseSkewOutputSystem" should "deskew outputs and emit parallel rows" in {
    test(new ReverseSkewOutputSystem(2, 2, fifoDepth = 8, cfg)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
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

      // Row 0 should emerge (after reverse-skew delay + FIFO enq)
      dut.io.out_row.valid.expect(true.B)
      dut.io.out_row.bits(0).expect(100.U)
      dut.io.out_row.bits(1).expect(101.U)
      dut.clock.step()

      // No more inputs
      dut.io.in_en(0).poke(false.B)
      dut.io.in_en(1).poke(false.B)

      // Row 1 should follow immediately
      dut.io.out_row.valid.expect(true.B)
      dut.io.out_row.bits(0).expect(140.U)
      dut.io.out_row.bits(1).expect(201.U)
      dut.clock.step()

      // Drained completely
      dut.io.out_row.valid.expect(false.B)
    }
  }
}
