// Asynchronous memory module

package dinocpu.memory

import chisel3._
import chisel3.util._

/**
 * Chisel enumerator to assign names to the UInt constants representing memory operations
 */
object MemoryOperation {
  val Read = 0.U(3.W)
  val Write = 1.U(3.W) 
  val ReadWrite = 2.U(3.W)
}
