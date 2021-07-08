package apps

import rise.core.DSL._
import rise.core.primitives.{let => _, _}
import rise.core.types._
import rise.openCL.primitives._
import shine.OpenCL.KernelExecutor.KernelNoSizes.fromKernelModule
import shine.OpenCL.{GlobalSize, HNilHelper, LocalSize, NDRange, ScalaFunction, `(`, `)=>`, `,`}
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


//  private val seqScanOcl = depFun((n:Nat) => depFun((m:Nat) => fun(ArrayType(n*m, f32))(input =>
//    let(input |> split(m) |> mapSeq(
//      oclScanSeqInclusive(AddressSpace.Private)(add)(lf32(0.0f))
//    ) |> oclToMem(AddressSpace.Global) |> map(drop(1))).be(scannedChunks => {
//
//      let(
//        scannedChunks |>
//          map(idx(lidx(m-1, m))) |> take(n-1) |>
//          oclScanSeqInclusive(AddressSpace.Private)(add)(lf32(0.0f)) |>
//          oclToMem(AddressSpace.Global)
//      ).be(scanOfLastElementOfChunks =>
//        zip(scannedChunks)(scanOfLastElementOfChunks) |>
//          map(fun(pair => pair._1 |> map(fun(x => x + pair._2)))) |>
//          join |> padCst(1)(0)(lf32(0.0f)) |> mapSeq(fun(x => x))
//      )
//    })
//  )))


  private val parallelScanOcl = depFun((n:Nat) => depFun((m:Nat) => fun(ArrayType(n*m, f32))(input =>
    let(input |> split(m) |> mapGlobal(0)(
      oclScanSeqInclusive(AddressSpace.Private)(add)(lf32(0.0f))
    ) |> oclToMem(AddressSpace.Global) |> map(drop(1))).be(scannedChunks => {

      let(
        scannedChunks |>
          map(idx(lidx(m-1, m))) |> take(n-1) |>
          oclScanSeqInclusive(AddressSpace.Private)(add)(lf32(0.0f)) |>
          oclToMem(AddressSpace.Global)
      ).be(scanOfLastElementOfChunks =>
        zip(scannedChunks)(scanOfLastElementOfChunks) |>
          map(fun(pair => pair._1 |> map(fun(x => x + pair._2)))) |>
          join |> padCst(1)(0)(lf32(0.0f)) |> mapGlobal(0)(fun(x => x))
        )
    })
  )))

  test("Parallel scan compiles to syntactically correct OpenCL") {
    util.withExecutor {
      val kernel = gen.opencl.kernel.fromExpr(parallelScanOcl)
      println(kernel.code)

      val run = kernel.as[ScalaFunction `(` Int `,` Int `,` Array[Float] `)=>` Array[Float]]

      val inputSize = 8192 * 8
      val m = 256 // Number of elements per thread/chunk
      val n = inputSize/m // Number of threads/chunks
      val data = Array.tabulate(n*m)(x => (x + 1.0f) % 32.0f) // Keep the numbers small

      println(Iterator(
        s"Input size: $inputSize floats",
        s"N (Num Threads): $n",
        s"M (Chunk size): $m"
      ).mkString("\n"))

      val localSize = LocalSize(NDRange(Math.min(n, 128), 1, 1))
      val globalSize = GlobalSize(NDRange(n, 1, 1))
      val (output, runtime) = run(localSize, globalSize)(n `,` m `,` data)

      println(s"Execution time: ${runtime.value} ms")

      val gold = data.scanLeft(0.0f)(_ + _)

      println(s"Output size: ${output.length}")
      //println(gold.iterator.map(_.toString).mkString(", "))
      //println(output.iterator.map(_.toString).mkString(", "))
      assert(gold.zip(output).forall(x => x._1 == x._2))
    }
  }
}
