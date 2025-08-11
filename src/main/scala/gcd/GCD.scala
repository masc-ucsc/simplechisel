// See README.md for license details.

package gcd

import chisel3._
// _root_ disambiguates from package chisel3.util.circt if user imports chisel3.util._
import _root_.circt.stage.ChiselStage

/**
  * Compute GCD using subtraction method.
  * Subtracts the smaller from the larger until register y is zero.
  * value in register x is then the GCD
  */
class GCD extends Module {
  val io = IO(new Bundle {
    val value1        = Input(UInt(16.W))
    val value2        = Input(UInt(16.W))
    val loadingValues = Input(Bool())
    val outputGCD     = Output(UInt(16.W))
    val outputValid   = Output(Bool())
  })

  val x  = Reg(UInt())
  val y  = Reg(UInt())

  when(x > y) { x := x - y }
    .otherwise { y := y - x }

  when(io.loadingValues) {
    x := io.value1
    y := io.value2
  }

  io.outputGCD := x
  io.outputValid := y === 0.U
}

/**
 * Generate Verilog sources and save it in file GCD.v
 */
import java.nio.file.{Files, Paths}

object GCD extends App {
  val outDirStr: String = sys.env.get("HAGENT_BUILD_DIR")
    .filter(_.nonEmpty)
    .map(dir => s"$dir/build_gcd")
    .getOrElse("./build_gcd")
  val outDir = Paths.get(outDirStr).toAbsolutePath.normalize

  // 2) Ensure the directory exists (idempotent).
  Files.createDirectories(outDir)

  // 3) Emit SystemVerilog into that directory.
  ChiselStage.emitSystemVerilogFile(
    new GCD,
    args = Array(
      "--target-dir", outDir.toString
    ),
    // Keep your FIRRTL/MLIR options unchanged.
    firtoolOpts = Array(
      "-disable-all-randomization",
      "-strip-debug-info",
      "-default-layer-specialization=enable"
    )
  )
}
