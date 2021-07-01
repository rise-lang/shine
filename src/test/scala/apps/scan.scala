package apps

import rise.core.DSL._
import rise.core.primitives._
import rise.core.types._
import rise.openCL.DSL
import rise.openCL.primitives._
import util.gen
import util.gen.c.function


class scan extends test_util.Tests {
  private val simpleScan = fun(ArrayType(8, f32))(array =>
    array |> scanSeq(add)(lf32(0.0f))
  )
  private val simpleScanOcl = fun(ArrayType(8, f32))(array =>
    array |> oclScanSeq(AddressSpace.Private)(add)(lf32(0.0f))
  )


  test("Simple scan compiles to syntactically correct C") {
    function.asStringFromExpr(simpleScan)
  }

  test("Simple scan compiles to syntactically correct OpenMP") {
    gen.openmp.function.asStringFromExpr(simpleScan)
  }

  // currently fails do to a missing address space at a new
  test("Simple scan compiles to syntactically correct OpenCL") {
    gen.opencl.kernel.asStringFromExpr(simpleScanOcl)
  }


  // PROBLEM: drop wants a read. scanSeq is a write. I don't want to materialize.
  private val parallelScanOcl = depFun((n:Nat) => depFun((m:Nat) => fun(ArrayType(n*m, f32))(input =>
    input |> split(m) |> mapSeq(oclScanSeqInclusive(AddressSpace.Private)(add)(lf32(0.0f)) >> drop(1))
  )))


  // Full template, currently not working
//  private val parallelScanOclEventual = depFun((n:Nat) => depFun((m:Nat) => fun(ArrayType(n*m, f32))(input =>
//    let(input |> split(m) |> mapGlobal(0)(oclScanSeqInclusive(AddressSpace.Private)(add)(lf32(0.0f)) >> drop(1))).be(
//      scannedChunks => {
//        val scanOfLastElements = scannedChunks |> mapGlobal(0)(fun(chunk =>
//          (chunk `@` (n-1)) |> take(m - 1) |> oclScanSeqInclusive(AddressSpace.Private)(add)(lf32(0.0f))
//        ))
//        scanOfLastElements
//      })
//  )))

  test("Parallel scan compiles to syntactically correct OpenCL") {
    val generated = gen.opencl.kernel.asStringFromExpr(parallelScanOcl)
    println(generated)
  }
}
