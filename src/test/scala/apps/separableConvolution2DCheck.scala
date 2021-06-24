package apps

import separableConvolution2D._
import rise.core._
import rise.core.types._
import rise.core.DSL._
import rise.core.primitives._
import Type._
import HighLevelConstructs._
import rise.core.DSL.ToBeTyped
import shine.OpenCL.KernelExecutor.KernelNoSizes.fromKernelModule
import util.gen
import util.gen.c.function

object separableConvolution2DCheck {
  def wrapExpr(e: ToBeTyped[Expr]): ToBeTyped[Expr] = {
    import arithexpr.arithmetic.{PosInf, RangeAdd}
    // at least 3 for one scalar sliding window
    // at least 3*4 = 12 for one vector sliding window
    depFun(RangeAdd(3, PosInf, 1), (h: Nat) =>
      depFun(RangeAdd(12, PosInf, 4), (w: Nat) =>
        fun(h`.`w`.`f32)(a => e(a))))
  }
}

class separableConvolution2DCheck extends test_util.Tests {
  import separableConvolution2DCheck._

  private val H = 20
  private val W = 80

  private def checkC(e: ToBeTyped[Expr]): Unit = {
    val random = new scala.util.Random()
    val input = Array.fill(H, W)(random.nextFloat())
    val gold = computeGold(H, W, input, binomialWeights2d)

    val compute = function("compute").asStringFromExpr(wrapExpr(e))
    val testCode =
      s"""
#include <stdio.h>

$compute

int main(int argc, char** argv) {
  float input[$H * $W] = { ${input.flatten.mkString(", ")} };

  float gold[$H * $W] = { ${gold.flatten.mkString(", ")} };

  float output[$H * $W];
  compute(output, $H, $W, input);

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
    e: ToBeTyped[Expr]
  ): Unit = {
    import shine.OpenCL._

    val random = new scala.util.Random()
    val input = Array.fill(H, W)(random.nextFloat())
    val gold = computeGold(H, W, input, binomialWeights2d).flatten

    val kernel = gen.opencl.kernel.fromExpr(wrapExpr(e))
    val run = kernel.as[ScalaFunction `(`
      Int `,` Int `,` Array[Array[Float]]
      `)=>` Array[Float]]
    val (output, time) = run(localSize, globalSize)(H `,` W `,` input)
    util.assertSame(output, gold, "output is different from gold")
    logger.debug(s"time: $time")
  }

  test("baseVecU compiles to valid OpenCL that passes checks") {
    test_util.withExecutor {
      checkOCL(LocalSize(1), GlobalSize(1), baseVecU(binomialWeights2d))
    }
  }

  test("regRotPar compiles to valid OpenCL that passes checks") {
    test_util.withExecutor {
      checkOCL(LocalSize(1), GlobalSize(4),
        regRotPar(binomialWeightsV)(binomialWeightsH)
      )
    }
  }

  test("scanlinePar compiles to valid OpenCL that passes checks") {
    test_util.withExecutor {
      checkOCL(LocalSize(1), GlobalSize(4),
        scanlinePar(binomialWeightsV)(binomialWeightsH)
      )
    }
  }

  test(
    "register rotation binomial with unroll should contain no modulo or division"
  ) {
    val id = fun(x => x)
    val e = padClamp2D(1) >> slide(3)(1) >> mapSeq(
      transpose >>
      map(dotSeqUnroll(binomialWeightsV)) >>
      rotateValues(3)(id) >>
      iterateStream(dotSeqUnroll(binomialWeightsH))
    )
    val code = function.asStringFromExpr(wrapExpr(e))
    " % ".r.findAllIn(code).length shouldBe 0
    " / ".r.findAllIn(code).length shouldBe 0
  }

  // FIXME: code generation cannot evaluate index literal
  ignore("compiling OpenCL private arrays should unroll loops") {
    import rise.openCL.DSL._
    import rise.openCL.primitives.oclReduceSeq

    val dotSeqPrivate = fun(a => fun(b =>
      zip(a)(b) |> map(mulT) |> oclReduceSeq(AddressSpace.Private)(add)(lf32(0.0f))
    ))

    val e = padClamp2D(1) >> slide2D(3, 1) >> mapGlobal(0)(mapGlobal(1)(
      toPrivateFun(mapSeq(dotSeqPrivate(binomialWeightsV))) >>
      dotSeqPrivate(binomialWeightsH)
    ))

    val code = gen.opencl.kernel.asStringFromExpr(wrapExpr(e))
    "for \\(".r.findAllIn(code).length shouldBe 2
  }
}
