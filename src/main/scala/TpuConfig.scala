import chisel3._
import chisel3.util._

// Configuration de base
abstract class TpuConfig {
  val dataWidth: Int
  val accWidth: Int
  def multiply(a: UInt, b: UInt): UInt
}

// Config pour l'Entier 8-bit
case class Int8Config() extends TpuConfig {
  val dataWidth = 8
  val accWidth = 32
  def multiply(a: UInt, b: UInt): UInt = (a.asSInt * b.asSInt).asUInt
}

// Config pour le BFloat16 (Simplifié)
case class BF16Config() extends TpuConfig {
  val dataWidth = 16
  val accWidth = 32
  def multiply(a: UInt, b: UInt): UInt = {
    val res = Wire(UInt(32.W))
    res := 0.U // TODO ADD BF16 MUL LOGIC
    res
  }
}
