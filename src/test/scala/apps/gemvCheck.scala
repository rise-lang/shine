package apps

import gemv._
import rise.core.DSL._
import Type._
import rise.autotune
import rise.core.Expr
import rise.core.types._
import util.{SyntaxChecker, gen}
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

  // host code generation fails
  test("hostcode generation for GEMV expressions fails"){

    def run(e: ToBeTyped[Expr]):String = {
      val wrapped = autotune.wrapOclRun(LocalSize(128), GlobalSize(1024))(e)
      val codeModule = gen.opencl.hosted.fromExpr(wrapped)
      shine.OpenCL.Module.translateToString(codeModule)
    }

    run(ocl.gemvFusedAMD)
    run(ocl.gemvKeplerBest)

  }

  test("generated hostcode fails"){
    val code =
      """
        |#include "ocl/ocl.h"
        |struct foo_t {
        |  Kernel k0;
        |};
        |
        |typedef struct foo_t foo_t;
        |
        |void foo_init(Context ctx, foo_t* self){
        |  (*self).k0 = loadKernel(ctx, k0);
        |}
        |
        |void foo_destroy(Context ctx, foo_t* self){
        |  destroyKernel(ctx, (*self).k0);
        |}
        |
        |void foo_run(Context ctx, foo_t* self, Buffer moutput, int n228, int n229, Buffer me230, Buffer me231, Buffer me232, float e233, float e234){
        |  {
        |    DeviceBuffer b0 = deviceBufferSync(ctx, moutput, n229 * sizeof(float), DEVICE_WRITE);
        |    DeviceBuffer b5 = deviceBufferSync(ctx, me231, n228 * sizeof(float), DEVICE_READ);
        |    DeviceBuffer b6 = deviceBufferSync(ctx, me230, n229 * (n228 * sizeof(float)), DEVICE_READ);
        |    DeviceBuffer b8 = deviceBufferSync(ctx, me232, n229 * sizeof(float), DEVICE_READ);
        |    const size_t global_size[3] = (const size_t[3]){1024, 1, 1};
        |    const size_t local_size[3] = (const size_t[3]){128, 1, 1};
        |    const KernelArg args[12] = (const KernelArg[12]){KARG(b0), KARG(n2n70), KARG(n2n69), KARG(n229), KARG(n228), KARG(b5), KARG(b6), KARG(e234), KARG(b8), KARG(e233), LARG(128 * sizeof(float)), LARG(1 * sizeof(float))};
        |    launchKernel(ctx, (*self).k0, global_size, local_size, 12, args);
        |  }
        |
        |}
        |
        |void foo_init_run(Context ctx, Buffer moutput, int n228, int n229, Buffer me230, Buffer me231, Buffer me232, float e233, float e234){
        |  foo_t foo;
        |  foo_init(ctx, &foo);
        |  foo_run(ctx, &foo, moutput, n228, n229, me230, me231, me232, e233, e234);
        |  foo_destroy(ctx, &foo);
        |}
        |
        |
        |""".stripMargin

    SyntaxChecker.apply(code)
  }
}
