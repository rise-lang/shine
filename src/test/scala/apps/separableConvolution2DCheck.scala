package apps

import separableConvolution2D._
import lift.core._
import lift.core.types._
import lift.core.primitives._
import lift.core.DSL._
import lift.core.HighLevelConstructs._
import util.gen

object separableConvolution2DCheck {
  def computeGold(h: Int, w: Int,
                  input: Array[Array[Float]],
                  weights: Array[Array[Float]]): Array[Array[Float]] = {
    val output = Array.fill(h, w)(Float.NaN)
    val lastY = h - 1
    val lastX = w - 1

    for (y <- input.indices) {
      val r0 = if (y > 0) input(y - 1) else input(0)
      val r1 = input(y)
      val r2 = if (y < lastY) input(y + 1) else input(lastY)
      for (x <- r1.indices) {
        val c0 = if (x > 0) x - 1 else 0
        val c1 = x
        val c2 = if (x < lastX) x + 1 else lastX
        output(y)(x) =
          weights(0)(0) * r0(c0) + weights(0)(1) * r0(c1) + weights(0)(2) * r0(c2) +
            weights(1)(0) * r1(c0) + weights(1)(1) * r1(c1) + weights(1)(2) * r1(c2) +
            weights(2)(0) * r2(c0) + weights(2)(1) * r2(c1) + weights(2)(2) * r2(c2)
      }
    }

    output
  }

  def computeGold(h: Int, w: Int,
                  input: Array[Array[Float]],
                  weights: Expr): Array[Array[Float]] = {
    import lift.core.semantics._
    weights match {
      case Literal(ArrayData(a)) => computeGold(h, w, input,
        a.map(r => r.asInstanceOf[ArrayData].a.map(x => x.asInstanceOf[FloatData].f).toArray).toArray)
      case _ => ???
    }
  }
}

class separableConvolution2DCheck extends test_util.Tests {
  import separableConvolution2DCheck._

  private def wrapExpr(e: Expr): Expr = {
    // at least 3 for one scalar sliding window
    // at least 3*4 = 12 for one vector sliding window
    val from = (n: Int) => lift.arithmetic.RangeAdd(n, lift.arithmetic.PosInf, 1)
    nFun(from(3), h => nFun(from(12), w => fun(h`.`w`.`float)(a => e(a))))
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

  import idealised.OpenCL.{LocalSize, GlobalSize}

  private def checkOCL(localSize: LocalSize,
                       globalSize: GlobalSize,
                       e: Expr): Unit = {
    import idealised.OpenCL._

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
      checkOCL(LocalSize(1), GlobalSize(1), regRotPar(binomialWeightsV)(binomialWeightsH))
    }
  }

  test("register rotation binomial with unroll should contain no modulo or division") {
    val id: Expr = fun(x => x)
    val e = padClamp2D(1) >> slide(3)(1) >> mapSeq(transpose >>
      map(dotSeqUnroll(binomialWeightsV)) >>
      slideSeq(slideSeq.Values)(3)(1)(id)(dotSeqUnroll(binomialWeightsH))
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
      toPrivateFun(mapSeq(dotSeqPrivate(binomialWeightsV))) >>
        dotSeqPrivate(binomialWeightsH)
    ))

    val code = gen.OpenCLKernel(wrapExpr(e), "blur").code
    "for \\(".r.findAllIn(code).length shouldBe 2
  }
}
