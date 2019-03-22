package idealised.apps

import idealised._
import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.Semantics._
import idealised.util.{Execute, SyntaxChecker}

object binomialFilter {
  // Binomial filter, convolution is separable:
  //
  // 1 2 1   1
  // 2 4 2 ~ 2 x 1 2 1
  // 1 2 1   1

  val mul2 = fun(t => t._1 * t._2)
  val add = fun(x => fun(a => x + a))
  val dot = fun(a => fun(b => zip(a, b) :>> map(mul2) :>> reduceSeq(add, 0.0f)))

  // TODO: pad
  // TODO: registers/loop unrolling, vectorisation
  // TODO: rewriting

  val weights2d = LiteralExpr(ArrayData(
    Array(1, 2, 1, 2, 4, 2, 1, 2, 1).map(f => FloatData(f / 16.0f))))
  val weights1d = LiteralExpr(ArrayData(Array(1, 2, 1).map(f => FloatData(f / 4.0f))))

  val slide2d = map(slide(3, 1)) >>> slide(3, 1) >>> map(transpose())
  def mapSeq2d(f: Expr) = mapSeq(mapSeq(f))

  val blur =
    nFun(h => nFun(w => fun(ArrayType(h, ArrayType(w, float)))(input =>
      input :>> /* pad2d >>> */ slide2d :>>
        mapSeq2d(fun(nbh => dot(weights2d)(join(nbh))))
    )))

  val factorised_blur =
    nFun(h => nFun(w => fun(ArrayType(h, ArrayType(w, float)))(input =>
      input :>> slide2d :>> mapSeq2d(mapSeq(dot(weights1d)) >>> dot(weights1d))
    )))

  val separated_blur = {
    val horizontal = mapSeq(slide(3, 1) >>> mapSeq(dot(weights1d)))
    val vertical = slide(3, 1) >>> mapSeq(transpose() >>> mapSeq(dot(weights1d)))
    nFun(h => nFun(w => fun(ArrayType(h, ArrayType(w, float)))(input =>
      input :>> vertical :>> horizontal
    )))
  }

  val regrot_blur =
    nFun(h => nFun(w => fun(ArrayType(h, ArrayType(w, float)))(input =>
      input :>> slide(3, 1) :>> mapSeq(
        transpose() >>> map(dot(weights1d)) >>> slideSeq(3, 1) >>> map(dot(weights1d))
      )
    )))
}

class binomialFilter extends idealised.util.Tests {
  def program(name: String, e: Expr): C.Program = {
    val phrase = idealised.DPIA.FromSurfaceLanguage(TypeInference(e, collection.Map()))
    val program = C.ProgramGenerator.makeCode(phrase, name)
    SyntaxChecker(program.code)
    println(program.code)
    program
  }

  lazy val ref_prog = program("blur_ref", binomialFilter.blur)
  test("blur compiles to C code") {
    ref_prog
  }

  def check_ref(e: Expr) = {
    val prog = program("blur", e)
    val testCode =
      s"""
#include <stdio.h>

${ref_prog.code}

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
  ${ref_prog.function.name}(reference, H, W, input);

  float output[H * W];
  ${prog.function.name}(output, H, W, input);

  for (int i = 0; i < (H * W); i++) {
    float delta = reference[i] - output[i];
    if (delta < -0.001 && 0.001 < delta) {
      fprintf(stderr, "difference with reference is too big: %f\\n", delta);
      return 1;
    }
  }

  return 0;
}
"""
    Execute(testCode)
  }

  test("compile and compare factorised blur to the reference") {
    check_ref(binomialFilter.factorised_blur)
  }

  test("compile and compare separated blur to the reference") {
    check_ref(binomialFilter.separated_blur)
  }

  test("compile and compare register rotation blur to the reference") {
    check_ref(binomialFilter.regrot_blur)
  }
}
