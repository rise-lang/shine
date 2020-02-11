package apps

import separableConvolution2D._
import rise.core._
import rise.core.types._
import rise.core.primitives._
import rise.core.DSL._
import rise.core.TypeLevelDSL._
import rise.core.HighLevelConstructs._
import util.gen

class separableConvolution2DCheck extends shine.test_util.Tests {
  private def wrapExpr(e: Expr): Expr = {
    import arithexpr.arithmetic.{PosInf, RangeAdd}
    // at least 3 for one scalar sliding window
    // at least 3*4 = 12 for one vector sliding window
    nFun(RangeAdd(3, PosInf, 1), h =>
      nFun(RangeAdd(12, PosInf, 4), w =>
        fun(h `.` w `.` f32)(a => e(a))))
  }

  private val H = 20
  private val W = 80

  private def checkC(e: Expr): Unit = {
    val random = new scala.util.Random()
    val input = Array.fill(H, W)(random.nextFloat)
    val gold = computeGold(H, W, input, binomialWeights2d)

    val prog = gen.CProgram(wrapExpr(e))
    val testCode =
      s"""
#include <stdio.h>

${prog.code}

int main(int argc, char** argv) {
  float input[$H * $W] = { ${input.flatten.mkString(", ")} };

  float gold[$H * $W] = { ${gold.flatten.mkString(", ")} };

  float output[$H * $W];
  ${prog.function.name}(output, $H, $W, input);

  for (int i = 0; i < ($H * $W); i++) {
    float delta = gold[i] - output[i];
    if (delta < -0.001 || 0.001 < delta) {
      fprintf(stderr, "difference with gold is too big: %f\\n", delta);
      return 1;
    }
  }

  return 0;
}
"""
    util.Execute(testCode)
  }

  test("baseSeq compiles to C Code that passes checks") {
    checkC(baseSeq(binomialWeights2d))
  }

  test("factorisedSeq compiles to C code that passes checks") {
    checkC(factorisedSeq(binomialWeightsV)(binomialWeightsH))
  }

  test("separatedSeq compiles to C code that passes checks") {
    checkC(separatedSeq(binomialWeightsV)(binomialWeightsH))
  }

  test("regRotSeq compiles to C code that passes checks") {
    checkC(regRotSeq(binomialWeightsV)(binomialWeightsH))
  }

  import shine.OpenCL.{GlobalSize, LocalSize}

  private def checkOCL(
    localSize: LocalSize,
    globalSize: GlobalSize,
    e: Expr
  ): Unit = {
    import shine.OpenCL._

    val random = new scala.util.Random()
    val input = Array.fill(H, W)(random.nextFloat)
    val gold = computeGold(H, W, input, binomialWeights2d).flatten

    val kernel = gen.OpenCLKernel(wrapExpr(e))
    val run = kernel.as[ScalaFunction `(`
      Int `,` Int `,` Array[Array[Float]]
      `)=>` Array[Float]]
    val (output, time) = run(localSize, globalSize)(H `,` W `,` input)
    util.assertSame(output, gold, "output is different from gold")
    println(s"time: $time")
  }

  test("regRotPar compiles to valid OpenCL that passes checks") {
    util.withExecutor {
      checkOCL(LocalSize(1), GlobalSize(4),
        regRotPar(binomialWeightsV)(binomialWeightsH)
      )
    }
  }

  test("scanlinePar compiles to valid OpenCL that passes checks") {
    util.withExecutor {
      checkOCL(LocalSize(1), GlobalSize(4),
        scanlinePar(binomialWeightsV)(binomialWeightsH)
      )
    }
  }

  test(
    "register rotation binomial with unroll should contain no modulo or division"
  ) {
    val id: Expr = fun(x => x)
    val e = padClamp2D(1) >> slide(3)(1) >> mapSeq(
      transpose >>
      map(dotSeqUnroll(binomialWeightsV)) >>
      slideSeq(SlideSeq.Values)(3)(1)(id)(dotSeqUnroll(binomialWeightsH))
    )
    val code = gen.CProgram(wrapExpr(e), "blur").code
    " % ".r.findAllIn(code).length shouldBe 0
    " / ".r.findAllIn(code).length shouldBe 0
  }

  test("compiling OpenCL private arrays should unroll loops") {
    import rise.OpenCL.DSL._

    val dotSeqPrivate = fun(a => fun(b =>
      zip(a)(b) |> map(mulT) |> oclReduceSeq(AddressSpace.Private)(add)(l(0.0f))
    ))

    val e = padClamp2D(1) >> slide2D(3, 1) >> mapGlobal(0)(mapGlobal(1)(
      toPrivateFun(mapSeq(dotSeqPrivate(binomialWeightsV))) >>
      dotSeqPrivate(binomialWeightsH)
    ))

    val code = gen.OpenCLKernel(wrapExpr(e), "blur").code
    "for \\(".r.findAllIn(code).length shouldBe 2
  }
}
