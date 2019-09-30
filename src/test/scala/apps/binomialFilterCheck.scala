package apps

import binomialFilter._
import lift.core._
import lift.core.types._
import lift.core.primitives._
import lift.core.DSL._
import lift.core.HighLevelConstructs._
import util.gen

class binomialFilterCheck extends test_util.Tests {
  private def wrapExpr(e: Expr): Expr = {
    val szr = lift.arithmetic.RangeAdd(3, lift.arithmetic.PosInf, 1)
    nFun(szr, h => nFun(szr, w => fun(h`.`w`.`float)(a => e(a))))
  }

  lazy val baseSeqC: idealised.C.Program =
    gen.CProgram(wrapExpr(baseSeq), "blur_base")

  test("baseSeq compiles to C Code") {
    baseSeqC
  }

  private def checkC(e: Expr) = {
    val prog = gen.CProgram(wrapExpr(e), "blur")
    val testCode =
      s"""
#include <stdio.h>

${baseSeqC.code}

${prog.code}

int main(int argc, char** argv) {
  const int H = 20;
  const int W = 80;

  float input[H * W];
  for (int y = 0; y < H; y++) {
    for (int x = 0; x < W; x++) {
      input[y * W + x] = 0.3f * y + 0.6f * x;
    }
  }

  float reference[H * W];
  ${baseSeqC.function.name}(reference, H, W, input);

  float output[H * W];
  ${prog.function.name}(output, H, W, input);

  for (int i = 0; i < (H * W); i++) {
    float delta = reference[i] - output[i];
    if (delta < -0.001 || 0.001 < delta) {
      fprintf(stderr, "difference with reference is too big: %f\\n", delta);
      return 1;
    }
  }

  return 0;
}
"""
    util.Execute(testCode)
  }

  test("factorisedSeq compiles to C code that passes checks") {
    checkC(factorisedSeq)
  }

  test("separatedSeq compiles to C code that passes checks") {
    checkC(separatedSeq)
  }

  test("regRotSeq compiles to C code that passes checks") {
    checkC(regRotSeq)
  }

  // TODO + check output
  ignore("regRotPar compiles to valid OpenCL") {
    gen.OpenCLKernel(wrapExpr(regRotPar))
  }

  test("register rotation blur with unroll should contain no modulo or division") {
    val id: Expr = fun(x => x)
    val e = padClamp2D(1) >> slide(3)(1) >> mapSeq(transpose >>
      map(dotSeqUnroll(weights1d)) >>
      slideSeq(slideSeq.Values)(3)(1)(id)(dotSeqUnroll(weights1d))
    )
    val code = gen.CProgram(wrapExpr(e), "blur").code
    " % ".r.findAllIn(code).length shouldBe 0
    " / ".r.findAllIn(code).length shouldBe 0
  }

  test("compiling OpenCL private arrays should unroll loops") {
    import lift.OpenCL.primitives._

    val dotSeqPrivate = fun(a => fun(b =>
      zip(a)(b) |> map(mulT) |> oclReduceSeq(AddressSpace.Private)(add)(l(0.0f))
    ))

    val e = padClamp2D(1) >> slide2D(3, 1) >> mapGlobal(0)(mapGlobal(1)(
      toPrivateFun(mapSeq(dotSeqPrivate(weights1d))) >>
        dotSeqPrivate(weights1d)
    ))

    val code = gen.OpenCLKernel(wrapExpr(e), "blur").code
    "for \\(".r.findAllIn(code).length shouldBe 2
  }

  test("compiling OpenCL shuffle should generate valid code") {
    import lift.OpenCL.primitives._

    val dotSeqVecUnroll = fun(a => fun(b =>
      zip(a)(b) |> map(mulT) |> oclReduceSeqUnroll(AddressSpace.Private)(add)(vectorFromScalar(l(0.0f)))
    ))

    val e = fun(ArrayType(3, VectorType(4, float)))(xs =>
      xs |> asScalar |> drop(3) |> take(6) |> slide(4)(1) |> join |> asVector(4)
        |> dotSeqVecUnroll(map(vectorFromScalar)(weights1d))
    )

    gen.OpenCLKernel(e)
  }
}
