package apps

import gemv._
import rise.core.DSL._
import Type._
import rise.autotune
import rise.core.Expr
import rise.core.types._
import util.{gen}
import util.gen.c.function
import rise.core.types.DataType._
import shine.OpenCL.{GlobalSize, LocalSize}

class gemvCheck extends test_util.Tests {
  private val N = 128
  private val M = 128

  test("high-level gemv type inference works") {
    val typed = gemvHighLevel.toExpr

    val N = typed.t.asInstanceOf[NatDepFunType[_ <: ExprType]].x
    val M = typed.t
      .asInstanceOf[NatDepFunType[_ <: ExprType]].t
      .asInstanceOf[NatDepFunType[_ <: ExprType]].x
    assertResult(
      DepFunType(NatKind, N,
        DepFunType(NatKind, M,
          ArrayType(M, ArrayType(N, f32)) ->:
            (ArrayType(N, f32) ->: (ArrayType(M, f32) ->:
            (f32 ->: (f32 ->: ArrayType(M, f32)))))
        ))) {
      typed.t
    }
  }

  test("sequential gemv compiles to syntactically correct C") {
    function.asStringFromExpr(gemvSequential)
  }

  test("OpenCL gemv versions type inference works") {
    ocl.gemvFused.toExpr
    ocl.gemvFusedAMD.toExpr
    ocl.gemvKeplerBest.toExpr
  }

  test("OpenCL gemv versions host-code generation creates syntactically correct host-code"){

    def run(e: ToBeTyped[Expr], localSize: LocalSize, globalSize: GlobalSize):String = {
      val wrapped = autotune.wrapOclRun(localSize, globalSize)(e)
      val codeModule = gen.opencl.hosted.fromExpr(wrapped)
      shine.OpenCL.Module.translateToString(codeModule) // syntax checker is called here
    }

    run(ocl.gemvBlastN, LocalSize(64), GlobalSize(1024))
    run(ocl.gemvBlastT, LocalSize(64), GlobalSize(1024))
    run(ocl.gemvFused, LocalSize(128), GlobalSize(1024))
    run(ocl.gemvFusedAMD, LocalSize(128), GlobalSize(1024))
    run(ocl.gemvKeplerBest, LocalSize(128), GlobalSize(1024))
  }

  test("OpenMP gemv versions type inference works") {
    omp.gemvFused.toExpr
  }

  test("OpenMP gemv versions compiles to syntactically correct OpenMP") {
    gen.openmp.function.asStringFromExpr(omp.gemvFused)
  }

  test("CGO17 CL Blast kernels produce expected result") {
    val rand = new scala.util.Random
    val mat = Array.fill(M, N)(rand.nextFloat() * 5)
    val matT = mat.transpose
    val xs = Array.fill(N)(rand.nextFloat() * 5)
    val ys = Array.fill(M)(rand.nextFloat() * 5)
    val alpha = rand.nextFloat() * 5
    val beta = rand.nextFloat() * 5

    val kernelN = gen.opencl.kernel(
      Some(gemvBlastKnowSizes),
      "KERNEL"
    ).fromExpr(ocl.gemvBlastN)
    val kernelT = gen.opencl.kernel(
      Some(gemvBlastKnowSizes),
      "KERNEL"
    ).fromExpr(ocl.gemvBlastT)

    util.withExecutor {
      runsWithSameResult(Seq(
        ("original N", runOriginal("CGO17_GEMV_N.cl", mat, xs, ys, alpha, beta)),
        ("dpia N", runKernel(kernelN, mat, xs, ys, alpha, beta)),
        ("original T", runOriginal("CGO17_GEMV_T.cl", matT, xs, ys, alpha, beta)),
        ("dpia T", runKernel(kernelT, matT, xs, ys, alpha, beta))
      ))
    }
  }
}
