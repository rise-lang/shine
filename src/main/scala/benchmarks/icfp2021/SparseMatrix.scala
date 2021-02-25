package benchmarks.icfp2021

import opencl.executor.Executor
import rise.core.Expr
import shine.OpenCL.KernelScalaInterop
import util.{Time, TimeSpan}

import java.io.File
import scala.util.Random


object SparseMatrix {
  def main(args: Array[String]): Unit = {
    Executor.loadAndInit()

    val files = Iterable(
//      "web-Google",
//      "web-Stanford",
//      "engine",
//      "GaAsH6",
//      "NACA0015",
//      "Goodwin_030",
//      "Goodwin_127",
//      "Dubcova3",
      "rajat30",
//      "kim2",
//      "gupta2",
//      "torso1",
//      "SiO2",
//      "pkustk12",
//      "mip1",
    )
    val results = files.map(name => {
      println(s"MATRIX: $name")
      val file = new File(s"/home/fedepiz/Desktop/artifact_sandbox/benchmark/matrix_cust/$name.mtx.cust")
      val matrix = TwoArrayCSR.fromCustomFormat(file)
      val utime = Kernels.spmv_unwrapped(matrix)
      val wtime = Kernels.spmv_wrapped(matrix)
      (name, utime, wtime)
    })
    Executor.shutdown()

    println("RESULTS:")
    results.foreach {
      case (name, utime, wtime) =>
        println(s"$name: $utime vs $wtime, ${if(wtime.value <= utime.value) "FASTER" else "SLOWER" }")
    }
  }
}

object Kernels {

  import rise.core.TypeLevelDSL._
  import rise.core.TypedDSL._
  import rise.core.primitives._
  import rise.core.types._
  import rise.openCL.primitives._
  import shine.OpenCL.{GlobalSize, HNilHelper, LocalSize, ScalaFunction, `(`, `)=>`, `,`}


  def csrMatrix(rows: Nat, cols:Nat, ns: NatCollection, et:ScalarType):DataType =
    rows `*.` (i => ((ns `@` (i+1)) - (ns `@` i)) `.` (IndexType(cols) x et))

  def wrappedCsrMatrix(rows: Nat, cols: Nat, et:ScalarType): Type =
    NatCollection ** (ns => csrMatrix(rows, cols, ns, et))

  def spmv_unwrapped(matrix:TwoArrayCSR): TimeSpan[Time.ms] = {
    val e = depFun((n:Nat) => depFun((m:Nat) => depFun((ns: NatCollection) =>
      fun(csrMatrix(n, m, ns, f32))(matrix => fun(m `.` f32)(vector =>
        matrix |> depMapGlobal(0)(depFun((i:Nat) => fun(row =>
          (row::(((ns `@` (i+1)) - (ns `@` i)) `.` (IndexType(m) x f32))) |>
            oclReduceSeq(AddressSpace.Private)(fun(acc => fun(y => acc + (y._2 * (vector `@` y._1)))))(l(0.0f))
        )))|> unDepArray)))))


    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)

    val kernel = util.gen.OpenCLKernel(inferred, "unwrapped_mv")
    val kernelF = kernel.as[ScalaFunction `(` Int `,` Int `,` Array[Int] `,` Array[Array[(Int, Float)]] `,` Array[Float] `)=>` Array[Float]]
      .withSizes(LocalSize(64), GlobalSize(8192))

    val rng = new Random()
    val vector = Array.tabulate(matrix.numCols)(_ => 1.0f)

    //val _ = kernelF(matrix.numRows `,` matrix.numCols `,` matrix.offsets `,` matrix.entries `,` vector)
    val (result, time) = kernelF(matrix.numRows `,` matrix.numCols `,` matrix.offsets `,` matrix.entries `,` vector)
    println(result.length)
    time
  }

  def spmv_wrapped(matrix: TwoArrayCSR): TimeSpan[Time.ms] = {
    val e = depFun((n:Nat) => depFun((m:Nat) => fun(wrappedCsrMatrix(n, m, f32))(matrixWrap => fun(m `.`f32)(vector => {
      dmatchNats(matrixWrap)(depFun((ns:NatCollection) => fun(csrMatrix(n, m, ns, f32))(matrix =>
        matrix |> depMapGlobal(0)(depFun((i:Nat) => fun(row =>
          (row::(((ns `@` (i+1)) - (ns `@` i)) `.` (IndexType(m) x f32))) |>
            oclReduceSeq(AddressSpace.Private)(fun(acc => fun(y => acc + (y._2 * (vector `@` y._1)))))(l(0.0f))
        )))|> unDepArray))
      )
    }))))


    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)

    val kernel = util.gen.OpenCLKernel(inferred, "wrapped")
    val kernelF = kernel.as[ScalaFunction `(` Int `,` Int `,`
      KernelScalaInterop.Puttable[KernelScalaInterop.DependentPair[Array[Array[(Int, Float)]]]] `,` Array[Float] `)=>` Array[Float]]
      .withSizes(LocalSize(64), GlobalSize(8192))

    val rng = new Random()
    val vector = Array.tabulate(matrix.numCols)(_ => 1.0f)

    val matrixArg = KernelScalaInterop.Puttable(
      KernelScalaInterop.DependentPair(matrix.offsets, matrix.entries)
    )

    //val _ = kernelF(matrix.numRows `,` matrix.numCols `,` matrixArg `,` vector)
    val (result, time) = kernelF(matrix.numRows `,` matrix.numCols `,` matrixArg `,` vector)
    println(result.length)
    time
  }


}
