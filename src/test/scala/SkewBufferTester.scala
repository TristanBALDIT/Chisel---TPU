import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SkewBufferTester extends AnyFlatSpec with ChiselScalatestTester {

  val cfg = Int8Config()

  "SkewBuffer" should "stagger inputs correctly" in {
    test(new SkewBuffer(3, cfg)) { dut =>
      // Cycle 0: Provide [10, 20, 30] at the same time
      dut.io.in(0).poke(10.U)
      dut.io.in(1).poke(20.U)
      dut.io.in(2).poke(30.U)

      // Check Row 0 immediately
      dut.io.out(0).expect(10.U)
      dut.io.out(1).expect(0.U)
      dut.io.out(2).expect(0.U)

      dut.clock.step()

      // Cycle 1 : stop input
      dut.io.in(0).poke(0.U)
      dut.io.in(1).poke(0.U)
      dut.io.in(2).poke(0.U)

      dut.io.out(0).expect(0.U)
      dut.io.out(1).expect(20.U)
      dut.io.out(2).expect(0.U)

      dut.clock.step()

      // Cycle 2 :
      dut.io.out(0).expect(0.U)
      dut.io.out(1).expect(0.U)
      dut.io.out(2).expect(30.U)

    }
  }
}
