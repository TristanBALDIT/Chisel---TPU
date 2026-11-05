import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class PETester extends AnyFlatSpec with ChiselScalatestTester {
  val cfg = Int8Config()

  "PE" should "load a weight and compute a MAC operation with a 1-cycle delay" in {
    test(new PE(cfg)) { dut =>

      // Load the Weight (W = 5)
      dut.io.load.poke(true.B)
      dut.io.in_w.poke(5.U)
      dut.clock.step()
      dut.io.load.poke(false.B)

      // Feed Activation (V = 10) and Partial Sum (Acc = 3)
      dut.io.en.poke(true.B)
      dut.io.in_v.poke(10.U)
      dut.io.in_acc.poke(3.U)
      dut.clock.step()

      // The MAC result takes one cycle due to RegNext
      // Verify MAC: 10 * 5 + 3 = 53
      dut.io.out_acc.expect(53.U)

      // Verify Activation passes through unchanged
      dut.io.out_v.expect(10.U)
    }
  }
}