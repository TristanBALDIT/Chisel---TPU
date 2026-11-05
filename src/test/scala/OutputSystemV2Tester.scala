import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class OutputSystemV2Tester extends AnyFlatSpec with ChiselScalatestTester {
  val cfg = Int8Config()

  "OutputSystemV2" should "absorb skewed inputs and serialize them round-robin" in {
    test(new OutputSystemV2(2, 2, cfg)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      dut.io.out_stream.ready.poke(true.B)

      // Cycle 0: Col 0 pushes its first valid result
      dut.io.in_res(0).poke(100.U)
      dut.io.in_en(0).poke(true.B)
      dut.io.in_en(1).poke(false.B)
      dut.clock.step()

      // Cycle 1: Col 0 pushes second result, Col 1 pushes first result
      dut.io.in_res(0).poke(140.U)
      dut.io.in_res(1).poke(140.U)
      dut.io.in_en(0).poke(true.B)
      dut.io.in_en(1).poke(true.B)

      // Pop 1: Col 0's first result
      dut.io.out_stream.valid.expect(true.B)
      dut.io.out_stream.bits.expect(100.U)

      dut.clock.step()

      // Cycle 2: Last input only on the second column
      dut.io.in_res(1).poke(200.U)
      dut.io.in_en(0).poke(false.B)
      dut.io.in_en(1).poke(true.B)

      // Pop 2: Col 1's first result
      dut.io.out_stream.valid.expect(true.B)
      dut.io.out_stream.bits.expect(140.U)

      dut.clock.step()

      // Cycle 3: No more input
      dut.io.in_en(0).poke(false.B)
      dut.io.in_en(1).poke(false.B)

      // Pop 3: Col 0's second result
      dut.io.out_stream.valid.expect(true.B)
      dut.io.out_stream.bits.expect(140.U)

      dut.clock.step()

      // Pop 4: Col 1's second result
      dut.io.out_stream.valid.expect(true.B)
      dut.io.out_stream.bits.expect(200.U)

      dut.clock.step()
    }
  }
}