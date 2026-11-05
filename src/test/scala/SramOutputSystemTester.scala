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

  it should "assert stall when SRAM buffer reaches capacity limit under backpressure and recover on drain" in {
    val totalRows = 8
    val depth = 8
    val cols = 2

    test(new SramOutputSystem(totalRows, cols, depth = depth, cfg)) { dut =>
      dut.io.out_row.ready.poke(false.B)
      dut.io.stall.expect(false.B)

      // Expected matrix: rows x cols
      val testData = Seq.tabulate(totalRows)(r => Seq.tabulate(cols)(c => (r + 1) * 100 + c))

      // Feed systolic inputs with skew:
      // Row r at column c arrives at cycle t = r + c
      val totalInputCycles = totalRows + cols - 1
      var stallObserved = false

      for (cycle <- 0 until totalInputCycles) {
        for (c <- 0 until cols) {
          val r = cycle - c
          if (r >= 0 && r < totalRows) {
            dut.io.in_en(c).poke(true.B)
            dut.io.in_res(c).poke(testData(r)(c).U)
          } else {
            dut.io.in_en(c).poke(false.B)
          }
        }

        if (dut.io.stall.peekBoolean()) {
          stallObserved = true
        }

        dut.clock.step()
      }

      // Stop all inputs
      for (c <- 0 until cols) {
        dut.io.in_en(c).poke(false.B)
      }

      // Verify that saturation was detected and stall remains asserted under continued backpressure
      assert(stallObserved, "Expected io.stall to be asserted when buffer reached capacity")
      dut.io.stall.expect(true.B)

      dut.clock.step()
      dut.io.stall.expect(true.B)

      // Downstream begins consuming
      dut.io.out_row.ready.poke(true.B)

      // Drain and verify all rows
      for (r <- 0 until totalRows) {
        while (!dut.io.out_row.valid.peekBoolean()) {
          dut.clock.step()
        }

        for (c <- 0 until cols) {
          dut.io.out_row.bits(c).expect(testData(r)(c).U, s"Row $r, Col $c mismatch")
        }

        dut.clock.step()
      }

      // Once drained, stall must be de-asserted and output queue empty
      dut.io.stall.expect(false.B)
      dut.io.out_row.valid.expect(false.B)
    }
  }

  it should "throttle upstream when stall is asserted and resume without data loss" in {
    val totalRows = 12
    val depth = 8
    val cols = 2

    test(new SramOutputSystem(totalRows, cols, depth = depth, cfg)) { dut =>
      val testData = Seq.tabulate(totalRows)(r => Seq.tabulate(cols)(c => (r + 1) * 20 + c))

      // Downstream initially applies backpressure
      dut.io.out_row.ready.poke(false.B)

      val colPtr = Array.fill(cols)(0)
      val skewCount = Array.fill(cols)(0)
      var stallCycles = 0
      var collectedRows = 0
      val received = scala.collection.mutable.ArrayBuffer[Seq[BigInt]]()

      var cycle = 0
      val maxCycles = 200

      while (collectedRows < totalRows && cycle < maxCycles) {
        cycle += 1

        val stall = dut.io.stall.peekBoolean()
        if (stall) {
          stallCycles += 1
        }

        // Downstream starts consuming after witnessing saturation
        if (stallCycles >= 3) {
          dut.io.out_row.ready.poke(true.B)
        }

        // Upstream systolic array model:
        // Pauses when dut.io.stall is active (flow control gating)
        if (stall) {
          for (c <- 0 until cols) {
            dut.io.in_en(c).poke(false.B)
          }
        } else {
          for (c <- 0 until cols) {
            if (skewCount(c) < c) {
              dut.io.in_en(c).poke(false.B)
              skewCount(c) += 1
            } else if (colPtr(c) < totalRows) {
              dut.io.in_en(c).poke(true.B)
              dut.io.in_res(c).poke(testData(colPtr(c))(c).U)
              colPtr(c) += 1
            } else {
              dut.io.in_en(c).poke(false.B)
            }
          }
        }

        // Collect outputs if ready and valid
        if (dut.io.out_row.valid.peekBoolean() && dut.io.out_row.ready.peekBoolean()) {
          val row = (0 until cols).map(c => dut.io.out_row.bits(c).peek().litValue)
          received += row
          collectedRows += 1
        }

        dut.clock.step()
      }

      assert(stallCycles >= 3, "Stall should have been asserted for at least 3 cycles")
      assert(collectedRows == totalRows, s"Expected $totalRows rows, got $collectedRows")

      for (r <- 0 until totalRows) {
        for (c <- 0 until cols) {
          assert(received(r)(c) == BigInt(testData(r)(c)), s"Mismatch at Row $r, Col $c: expected ${testData(r)(c)}, got ${received(r)(c)}")
        }
      }

      dut.io.stall.expect(false.B)
    }
  }

  it should "handle sustained saturation boundaries with alternating backpressure and pointer wraparounds" in {
    val totalRows = 16
    val depth = 8
    val cols = 2

    test(new SramOutputSystem(totalRows, cols, depth = depth, cfg)) { dut =>
      val testData = Seq.tabulate(totalRows)(r => Seq.tabulate(cols)(c => (r + 1) * 15 + c))

      val colPtr = Array.fill(cols)(0)
      val skewCount = Array.fill(cols)(0)
      var collectedRows = 0
      val received = scala.collection.mutable.ArrayBuffer[Seq[BigInt]]()
      var stallAssertedCount = 0

      var cycle = 0
      val maxCycles = 300

      while (collectedRows < totalRows && cycle < maxCycles) {
        cycle += 1

        val stall = dut.io.stall.peekBoolean()
        if (stall) {
          stallAssertedCount += 1
        }

        // Intermittent downstream backpressure:
        // ready is true for 2 cycles, false for 4 cycles
        val downstreamReady = (cycle % 6) < 2
        dut.io.out_row.ready.poke(downstreamReady.B)

        // Upstream stalls when backpressure generates stall signal
        if (stall) {
          for (c <- 0 until cols) {
            dut.io.in_en(c).poke(false.B)
          }
        } else {
          for (c <- 0 until cols) {
            if (skewCount(c) < c) {
              dut.io.in_en(c).poke(false.B)
              skewCount(c) += 1
            } else if (colPtr(c) < totalRows) {
              dut.io.in_en(c).poke(true.B)
              dut.io.in_res(c).poke(testData(colPtr(c))(c).U)
              colPtr(c) += 1
            } else {
              dut.io.in_en(c).poke(false.B)
            }
          }
        }

        if (dut.io.out_row.valid.peekBoolean() && dut.io.out_row.ready.peekBoolean()) {
          val row = (0 until cols).map(c => dut.io.out_row.bits(c).peek().litValue)
          received += row
          collectedRows += 1
        }

        dut.clock.step()
      }

      assert(stallAssertedCount > 0, "Stall signal should have asserted multiple times during heavy backpressure")
      assert(collectedRows == totalRows, s"Expected $totalRows rows, got $collectedRows")

      for (r <- 0 until totalRows) {
        for (c <- 0 until cols) {
          assert(received(r)(c) == BigInt(testData(r)(c)), s"Mismatch at Row $r, Col $c: expected ${testData(r)(c)}, got ${received(r)(c)}")
        }
      }

      dut.io.stall.expect(false.B)
    }
  }

  it should "saturate and recover cleanly with a 3x3 array configuration" in {
    val totalRows = 8
    val depth = 8
    val cols = 3

    test(new SramOutputSystem(totalRows, cols, depth = depth, cfg)) { dut =>
      dut.io.out_row.ready.poke(false.B)
      dut.io.stall.expect(false.B)

      val testData = Seq.tabulate(totalRows)(r => Seq.tabulate(cols)(c => (r + 1) * 30 + c))
      val totalInputCycles = totalRows + cols - 1
      var stallObserved = false

      for (cycle <- 0 until totalInputCycles) {
        for (c <- 0 until cols) {
          val r = cycle - c
          if (r >= 0 && r < totalRows) {
            dut.io.in_en(c).poke(true.B)
            dut.io.in_res(c).poke(testData(r)(c).U)
          } else {
            dut.io.in_en(c).poke(false.B)
          }
        }

        if (dut.io.stall.peekBoolean()) {
          stallObserved = true
        }

        dut.clock.step()
      }

      for (c <- 0 until cols) {
        dut.io.in_en(c).poke(false.B)
      }

      assert(stallObserved, "Expected io.stall to be asserted on 3x3 configuration under saturation")
      dut.io.stall.expect(true.B)

      // Consume output
      dut.io.out_row.ready.poke(true.B)

      for (r <- 0 until totalRows) {
        while (!dut.io.out_row.valid.peekBoolean()) {
          dut.clock.step()
        }

        for (c <- 0 until cols) {
          dut.io.out_row.bits(c).expect(testData(r)(c).U, s"Row $r, Col $c mismatch")
        }

        dut.clock.step()
      }

      dut.io.stall.expect(false.B)
      dut.io.out_row.valid.expect(false.B)
    }
  }
}
