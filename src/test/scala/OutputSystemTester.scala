import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class OutputSystemTester extends AnyFlatSpec with ChiselScalatestTester {

  // Assuming Int8Config defines accWidth as 32 or similar
  val cfg = Int8Config()

  "OutputSystem" should "offload data correctly" in {
    test(new OutputSystem(3, cfg)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      // 1. Initialize inputs
      dut.io.in_en.poke(false.B)
      dut.io.out_stream.ready.poke(false.B)
      dut.clock.step()

      // 2. Load parallel data into the FIFOs
      // Let's push values: Col0=10, Col1=20, Col2=30
      val testValues = Seq(10, 20, 30)
      for (i <- 0 until 3) {
        dut.io.in_res(i).poke(testValues(i).U)
      }
      dut.io.in_en.poke(true.B)
      dut.clock.step() // Data is now in Queues

      dut.io.in_en.poke(false.B) // Stop enqueueing
      dut.clock.step()

      // 3. Drain and Verify Round-Robin Logic
      dut.io.out_stream.ready.poke(true.B)

      for (expectedVal <- testValues) {
        // Wait for valid (it should be immediate here as Queues aren't empty)
        dut.io.out_stream.valid.expect(true.B)
        dut.io.out_stream.bits.expect(expectedVal.U)
        dut.clock.step()
      }

      // 4. Verify stream is empty/invalid after drain
      dut.io.out_stream.valid.expect(false.B)
    }
  }
}