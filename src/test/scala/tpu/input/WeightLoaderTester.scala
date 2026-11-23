package tpu.input

import tpu.config._
import tpu.core.SystolicArray
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

/**
 * Test harness wiring a Dual-Port Weight BRAM (SyncReadMem) -> WeightLoader -> SystolicArray
 */
private class WeightLoaderSystolicHarness(config: TpuConfig) extends Module {
  val rows = config.rows
  val cols = config.cols

  val io = IO(new Bundle {
    // CPU Port A: Write weights into Weight BRAM
    val cpu_wr_en   = Input(Bool())
    val cpu_wr_addr = Input(UInt(config.weightAddrWidth.W))
    val cpu_wr_data = Input(Vec(cols, UInt(config.dataWidth.W)))

    // MMIO Control for WeightLoader
    val start    = Input(Bool())
    val baseAddr = Input(UInt(config.weightAddrWidth.W))
    val matrixK  = Input(UInt(config.kWidth.W))
    val matrixN  = Input(UInt(config.nWidth.W))
    val busy     = Output(Bool())
    val done     = Output(Bool())

    // SystolicArray activation & output ports
    val stall      = Input(Bool())
    val in_v       = Input(Vec(rows, UInt(config.dataWidth.W)))
    val in_v_valid = Input(Vec(rows, Bool()))
    val out_res    = Output(Vec(cols, UInt(config.accWidth.W)))
    val out_valid  = Output(Vec(cols, Bool()))
  })

  // Dual-Port Weight BRAM (one SyncReadMem bank per column, matching SramOutputSystem style)
  val weightBanks = Seq.fill(cols)(SyncReadMem(config.weightBramDepth, UInt(config.dataWidth.W)))
  for (c <- 0 until cols) {
    when(io.cpu_wr_en) {
      weightBanks(c).write(io.cpu_wr_addr, io.cpu_wr_data(c))
    }
  }

  val loader = Module(new WeightLoader(config))
  val array  = Module(new SystolicArray(rows, cols, config))

  // Connect MMIO to WeightLoader
  loader.io.start    := io.start
  loader.io.baseAddr := io.baseAddr
  loader.io.matrixK  := io.matrixK
  loader.io.matrixN  := io.matrixN
  loader.io.stall    := io.stall
  io.busy            := loader.io.busy
  io.done            := loader.io.done

  // Connect Weight BRAM Port B to WeightLoader (exact BRAM address width from config!)
  for (c <- 0 until cols) {
    loader.io.mem_data(c) := weightBanks(c).read(loader.io.mem_addr, loader.io.mem_en)
  }

  // Connect WeightLoader directly to SystolicArray top edge
  array.io.load       := loader.io.out_load
  array.io.in_w       := loader.io.out_w
  array.io.in_w_valid := loader.io.out_w_valid
  array.io.stall      := io.stall

  // Connect SystolicArray left/bottom edges to harness IO
  array.io.in_v       := io.in_v
  array.io.in_v_valid := io.in_v_valid
  io.out_res          := array.io.out_res
  io.out_valid        := array.io.out_valid
}

class WeightLoaderTester extends AnyFlatSpec with ChiselScalatestTester {
  val cfg2x2 = Int8Config(rows = 2, cols = 2, weightBramDepth = 16, actBramDepth = 16)
  val cfg3x3 = Int8Config(rows = 3, cols = 3, weightBramDepth = 16, actBramDepth = 16)

  "WeightLoader" should "fetch weight rows bottom-to-top from BRAM and drive SystolicArray load signals" in {
    test(new WeightLoader(cfg2x2)) { dut =>
      // Weight BRAM (row-major for C = A * W, where W is K=2 x N=2):
      // W = | 1  2 | -> Addr 10 (Row 0 of W -> PE Row 0): [1, 2]
      //     | 3  4 | -> Addr 11 (Row 1 of W -> PE Row 1): [3, 4]
      val mem = Map(
        10 -> Seq(1, 2),
        11 -> Seq(3, 4)
      )

      dut.io.stall.poke(false.B)
      dut.io.start.poke(false.B)
      dut.io.baseAddr.poke(10.U)
      dut.io.matrixK.poke(2.U)
      dut.io.matrixN.poke(2.U)
      dut.io.mem_data(0).poke(0.U)
      dut.io.mem_data(1).poke(0.U)
      dut.clock.step()

      // Pulse start
      dut.io.start.poke(true.B)
      dut.clock.step()
      dut.io.start.poke(false.B)

      // Cycle 1: Request Row 1 first (baseAddr + 1 = 11)
      dut.io.busy.expect(true.B)
      dut.io.done.expect(false.B)
      dut.io.mem_en.expect(true.B)
      dut.io.mem_addr.expect(11.U)
      dut.io.out_load.expect(false.B)
      dut.clock.step()

      // Cycle 2: BRAM returns Row 1 ([3, 4]); controller requests Row 0 (addr 10)
      dut.io.mem_data(0).poke(mem(11)(0).U)
      dut.io.mem_data(1).poke(mem(11)(1).U)
      dut.io.mem_en.expect(true.B)
      dut.io.mem_addr.expect(10.U)
      dut.io.out_load.expect(true.B)
      dut.io.out_w(0).expect(3.U); dut.io.out_w_valid(0).expect(true.B)
      dut.io.out_w(1).expect(4.U); dut.io.out_w_valid(1).expect(true.B)
      dut.clock.step()

      // Cycle 3: BRAM returns Row 0 ([1, 2]); no more BRAM reads, last shift into mesh
      dut.io.mem_data(0).poke(mem(10)(0).U)
      dut.io.mem_data(1).poke(mem(10)(1).U)
      dut.io.mem_en.expect(false.B)
      dut.io.out_load.expect(true.B)
      dut.io.out_w(0).expect(1.U); dut.io.out_w_valid(0).expect(true.B)
      dut.io.out_w(1).expect(2.U); dut.io.out_w_valid(1).expect(true.B)
      dut.clock.step()

      // Cycle 4: Done pulsed, out_load deasserted
      dut.io.mem_data(0).poke(0.U)
      dut.io.mem_data(1).poke(0.U)
      dut.io.out_load.expect(false.B)
      dut.io.out_w_valid(0).expect(false.B)
      dut.io.out_w_valid(1).expect(false.B)
      dut.io.done.expect(true.B)
      dut.io.busy.expect(false.B)
      dut.clock.step()

      // Cycle 5: Idle
      dut.io.done.expect(false.B)
      dut.io.busy.expect(false.B)
    }
  }

  it should "mask inactive rows and columns for sub-matrices and flush lower PE rows" in {
    test(new WeightLoader(cfg3x3)) { dut =>
      // 1x2 sub-matrix W (K = 1 row, N = 2 cols, row-major) inside a 3x3 array:
      // W = [5, 7], stored at Addr 0 (PE Row 0): [5, 7, 99] -> PE Col 2 (99) masked out
      dut.io.stall.poke(false.B)
      dut.io.start.poke(false.B)
      dut.io.baseAddr.poke(0.U)
      dut.io.matrixK.poke(1.U)
      dut.io.matrixN.poke(2.U)
      for (c <- 0 until 3) dut.io.mem_data(c).poke(0.U)
      dut.clock.step()

      dut.io.start.poke(true.B)
      dut.clock.step()
      dut.io.start.poke(false.B)

      // Cycle 1: reqRow = 2 (>= K=1) -> no BRAM read
      dut.io.mem_en.expect(false.B)
      dut.io.out_load.expect(false.B)
      dut.clock.step()

      // Cycle 2: reqRow = 1 (>= K=1) -> no BRAM read; shifts invalid flush row for PE Row 2
      dut.io.mem_en.expect(false.B)
      dut.io.out_load.expect(true.B)
      for (c <- 0 until 3) {
        dut.io.out_w(c).expect(0.U)
        dut.io.out_w_valid(c).expect(false.B)
      }
      dut.clock.step()

      // Cycle 3: reqRow = 0 (< K=1) -> issues BRAM read at addr 0; shifts invalid flush row for PE Row 1
      dut.io.mem_en.expect(true.B)
      dut.io.mem_addr.expect(0.U)
      dut.io.out_load.expect(true.B)
      for (c <- 0 until 3) {
        dut.io.out_w(c).expect(0.U)
        dut.io.out_w_valid(c).expect(false.B)
      }
      dut.clock.step()

      // Cycle 4: BRAM returns [5, 7, 99]; shifts Row 0 with Col 2 masked out
      dut.io.mem_data(0).poke(5.U)
      dut.io.mem_data(1).poke(7.U)
      dut.io.mem_data(2).poke(99.U)
      dut.io.mem_en.expect(false.B)
      dut.io.out_load.expect(true.B)
      dut.io.out_w(0).expect(5.U); dut.io.out_w_valid(0).expect(true.B)
      dut.io.out_w(1).expect(7.U); dut.io.out_w_valid(1).expect(true.B)
      dut.io.out_w(2).expect(0.U); dut.io.out_w_valid(2).expect(false.B)
      dut.clock.step()

      // Cycle 5: Done
      dut.io.out_load.expect(false.B)
      dut.io.done.expect(true.B)
      dut.io.busy.expect(false.B)
    }
  }

  it should "freeze weight loading cleanly when stall is asserted" in {
    test(new WeightLoader(cfg2x2)) { dut =>
      dut.io.stall.poke(false.B)
      dut.io.start.poke(false.B)
      dut.io.baseAddr.poke(0.U)
      dut.io.matrixK.poke(2.U)
      dut.io.matrixN.poke(2.U)
      dut.io.mem_data(0).poke(0.U)
      dut.io.mem_data(1).poke(0.U)
      dut.clock.step()

      dut.io.start.poke(true.B)
      dut.clock.step()
      dut.io.start.poke(false.B)

      // Cycle 1: Request addr 1
      dut.io.mem_en.expect(true.B)
      dut.io.mem_addr.expect(1.U)
      dut.clock.step()

      // Cycle 2: BRAM returns [3, 4], assert stall
      dut.io.mem_data(0).poke(3.U)
      dut.io.mem_data(1).poke(4.U)
      dut.io.stall.poke(true.B)

      dut.io.mem_en.expect(false.B)
      dut.io.out_load.expect(true.B)
      dut.io.out_w(0).expect(3.U); dut.io.out_w_valid(0).expect(true.B)
      dut.io.out_w(1).expect(4.U); dut.io.out_w_valid(1).expect(true.B)

      // Hold stall for 3 cycles
      dut.clock.step(3)
      dut.io.busy.expect(true.B)
      dut.io.done.expect(false.B)
      dut.io.out_load.expect(true.B)
      dut.io.out_w(0).expect(3.U)
      dut.io.out_w(1).expect(4.U)

      // Unfreeze: addr 0 is now requested while [3, 4] is shifted in
      dut.io.stall.poke(false.B)
      dut.io.mem_en.expect(true.B)
      dut.io.mem_addr.expect(0.U)
      dut.clock.step()

      // Next cycle: BRAM returns [1, 2]
      dut.io.mem_data(0).poke(1.U)
      dut.io.mem_data(1).poke(2.U)
      dut.io.out_load.expect(true.B)
      dut.io.out_w(0).expect(1.U); dut.io.out_w_valid(0).expect(true.B)
      dut.io.out_w(1).expect(2.U); dut.io.out_w_valid(1).expect(true.B)
      dut.clock.step()

      // Done
      dut.io.out_load.expect(false.B)
      dut.io.done.expect(true.B)
    }
  }

  it should "integrate with Weight BRAM and SystolicArray for full and sub-matrix reloads" in {
    test(new WeightLoaderSystolicHarness(cfg2x2)) { dut =>
      dut.io.stall.poke(false.B)
      dut.io.cpu_wr_en.poke(false.B)
      dut.io.start.poke(false.B)
      dut.io.in_v(0).poke(0.U); dut.io.in_v_valid(0).poke(false.B)
      dut.io.in_v(1).poke(0.U); dut.io.in_v_valid(1).poke(false.B)
      dut.clock.step()

      // --- 1. CPU writes 2x2 weights W (K=2, N=2) row-major to BRAM ---
      // W = | 1  2 |  -> Addr 0 (Row 0 of W -> PE Row 0): [1, 2]
      //     | 3  4 |  -> Addr 1 (Row 1 of W -> PE Row 1): [3, 4]
      dut.io.cpu_wr_en.poke(true.B)
      dut.io.cpu_wr_addr.poke(0.U)
      dut.io.cpu_wr_data(0).poke(1.U); dut.io.cpu_wr_data(1).poke(2.U)
      dut.clock.step()

      dut.io.cpu_wr_addr.poke(1.U)
      dut.io.cpu_wr_data(0).poke(3.U); dut.io.cpu_wr_data(1).poke(4.U)
      dut.clock.step()
      dut.io.cpu_wr_en.poke(false.B)

      // --- 2. Trigger WeightLoader (K=2, N=2) ---
      dut.io.baseAddr.poke(0.U)
      dut.io.matrixK.poke(2.U)
      dut.io.matrixN.poke(2.U)
      dut.io.start.poke(true.B)
      dut.clock.step()
      dut.io.start.poke(false.B)

      while (!dut.io.done.peekBoolean()) {
        dut.clock.step()
      }

      // --- 3. Stream skewed activations A (M=2 x K=2, row-major) = | 10 30 | / | 20 40 | ---
      dut.io.in_v(0).poke(10.U); dut.io.in_v_valid(0).poke(true.B)
      dut.io.in_v(1).poke(0.U);  dut.io.in_v_valid(1).poke(false.B)
      dut.clock.step()

      dut.io.in_v(0).poke(20.U); dut.io.in_v_valid(0).poke(true.B)
      dut.io.in_v(1).poke(30.U); dut.io.in_v_valid(1).poke(true.B)
      dut.clock.step()

      dut.io.in_v(0).poke(0.U);  dut.io.in_v_valid(0).poke(false.B)
      dut.io.in_v(1).poke(40.U); dut.io.in_v_valid(1).poke(true.B)
      // Col 0: 1*10 + 3*30 = 100
      dut.io.out_res(0).expect(100.U); dut.io.out_valid(0).expect(true.B)
      dut.io.out_valid(1).expect(false.B)
      dut.clock.step()

      dut.io.in_v(1).poke(0.U); dut.io.in_v_valid(1).poke(false.B)
      // Col 0: 1*20 + 3*40 = 140 | Col 1: 2*10 + 4*30 = 140
      dut.io.out_res(0).expect(140.U); dut.io.out_valid(0).expect(true.B)
      dut.io.out_res(1).expect(140.U); dut.io.out_valid(1).expect(true.B)
      dut.clock.step()

      // Col 1: 2*20 + 4*40 = 200
      dut.io.out_valid(0).expect(false.B)
      dut.io.out_res(1).expect(200.U); dut.io.out_valid(1).expect(true.B)
      dut.clock.step()

      // --- 4. Reload a smaller 1x2 sub-matrix W (K=1, N=2, row-major) at addr 4 without reset ---
      // A = [10], W = [5, 7] -> C = A * W = [50, 70]
      // Proves stale weights [3, 4] in PE Row 1 are flushed with weight_valid = false
      dut.io.cpu_wr_en.poke(true.B)
      dut.io.cpu_wr_addr.poke(4.U)
      dut.io.cpu_wr_data(0).poke(5.U); dut.io.cpu_wr_data(1).poke(7.U)
      dut.clock.step()
      dut.io.cpu_wr_en.poke(false.B)

      dut.io.baseAddr.poke(4.U)
      dut.io.matrixK.poke(1.U)
      dut.io.matrixN.poke(2.U)
      dut.io.start.poke(true.B)
      dut.clock.step()
      dut.io.start.poke(false.B)

      while (!dut.io.done.peekBoolean()) {
        dut.clock.step()
      }

      // Stream activation 10 on Row 0; inject garbage 99 on Row 1 (must be bypassed!)
      dut.io.in_v(0).poke(10.U); dut.io.in_v_valid(0).poke(true.B)
      dut.io.in_v(1).poke(0.U);  dut.io.in_v_valid(1).poke(false.B)
      dut.clock.step()

      dut.io.in_v(0).poke(0.U);  dut.io.in_v_valid(0).poke(false.B)
      dut.io.in_v(1).poke(99.U); dut.io.in_v_valid(1).poke(true.B)
      dut.clock.step()

      dut.io.in_v(1).poke(0.U); dut.io.in_v_valid(1).poke(false.B)
      // Col 0: 10 * 5 = 50 (Row 1 bypassed 99 cleanly)
      dut.io.out_res(0).expect(50.U); dut.io.out_valid(0).expect(true.B)
      dut.io.out_valid(1).expect(false.B)
      dut.clock.step()

      // Col 1: 10 * 7 = 70
      dut.io.out_valid(0).expect(false.B)
      dut.io.out_res(1).expect(70.U); dut.io.out_valid(1).expect(true.B)
      dut.clock.step()

      dut.io.out_valid(0).expect(false.B)
      dut.io.out_valid(1).expect(false.B)
    }
  }
}
