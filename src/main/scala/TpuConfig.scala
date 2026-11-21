import chisel3._
import chisel3.util._

/**
 * TPU Matrix Multiplication Convention: C = A * W  (Google TPU / Gemmini Weight-Stationary Standard)
 *
 * Dimensions:
 * - Activation Matrix A : M x K  (M rows/batch x K input features)  -> Streams from left
 * - Weight Matrix     W : K x N  (K input features x N out features) -> Stationary in PEs
 * - Output Matrix     C : M x N  (M rows/batch x N out features)     -> Emitted at bottom
 *
 * Hardware Mapping (Weight-Stationary Systolic Array of size rows x cols):
 * - M (<= actBramDepth) : Batch / sequence length   -> Streaming timesteps
 * - K (<= rows)         : Inner dot-product dim     -> Active PE rows (0 .. K-1)
 * - N (<= cols)         : Output width              -> Active PE cols (0 .. N-1)
 *
 * Memory Layout (100% Natural Row-Major — Zero Transposes):
 * - Activation BRAM slot m (0 <= m < M) : Row m of A = [A(m,0), A(m,1), ..., A(m,K-1)] -> Timestep m
 * - Weight BRAM slot k (0 <= k < K)     : Row k of W = [W(k,0), W(k,1), ..., W(k,N-1)] -> PE Row k
 * - Output vector m (0 <= m < M)        : Row m of C = [C(m,0), C(m,1), ..., C(m,N-1)] -> Out row m
 */
abstract class TpuConfig {
  val dataWidth: Int
  val accWidth: Int

  // Hardware array & BRAM geometry
  val rows: Int
  val cols: Int
  val weightBramDepth: Int
  val actBramDepth: Int

  // Derived bit-widths (single source of truth across all TPU modules)
  // M <= actBramDepth (rows of A = rows of C), K <= rows (cols of A = rows of W), N <= cols (cols of W = cols of C)
  def kWidth: Int          = log2Ceil(rows + 1)
  def nWidth: Int          = log2Ceil(cols + 1)
  def mWidth: Int          = log2Ceil(actBramDepth + 1)
  def weightAddrWidth: Int = log2Ceil(weightBramDepth.max(2))
  def actAddrWidth: Int    = log2Ceil(actBramDepth.max(2))

  def multiply(a: UInt, b: UInt): UInt
}

// Config pour l'Entier 8-bit
case class Int8Config(
  rows: Int            = 8,
  cols: Int            = 8,
  weightBramDepth: Int = 512,
  actBramDepth: Int    = 1024
) extends TpuConfig {
  require(rows > 0 && cols > 0, "rows and cols must be positive")
  require(weightBramDepth >= rows.max(2), "weightBramDepth must be at least max(rows, 2)")
  require(actBramDepth >= 2, "actBramDepth must be at least 2")

  val dataWidth = 8
  val accWidth  = 32
  def multiply(a: UInt, b: UInt): UInt = (a.asSInt * b.asSInt).asUInt
}

// Config pour le BFloat16 (Simplifié)
case class BF16Config(
  rows: Int            = 8,
  cols: Int            = 8,
  weightBramDepth: Int = 512,
  actBramDepth: Int    = 1024
) extends TpuConfig {
  require(rows > 0 && cols > 0, "rows and cols must be positive")
  require(weightBramDepth >= rows.max(2), "weightBramDepth must be at least max(rows, 2)")
  require(actBramDepth >= 2, "actBramDepth must be at least 2")

  val dataWidth = 16
  val accWidth  = 32
  def multiply(a: UInt, b: UInt): UInt = {
    val res = Wire(UInt(32.W))
    res := 0.U // TODO ADD BF16 MUL LOGIC
    res
  }
}
