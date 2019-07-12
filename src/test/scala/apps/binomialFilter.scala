package apps

import lift.core._
import lift.core.primitives._
import lift.core.semantics._
import lift.core.DSL._
import lift.core.types._
import lift.core.HighLevelConstructs.padClamp2D

import elevate.core._
import elevate.core.strategies.normalforms._
import elevate.core.strategies.basic._
import rules._
import rules.algorithmic._
import rules.movement._
import strategies.algorithmic._

import idealised.C
import idealised.util.{Execute, SyntaxChecker}

object binomialFilter {
  // Binomial filter, convolution is separable:
  //
  // 1 2 1   1
  // 2 4 2 ~ 2 x 1 2 1
  // 1 2 1   1

  val mulT = fun(x => fst(x) * snd(x))
  val add = fun(x => fun(a => x + a))
  val dot = fun(a => fun(b =>
    zip(a)(b) |> map(mulT) |> reduce(add)(l(0.0f))
  ))
  val dotSeq = fun(a => fun(b =>
    zip(a)(b) |> map(mulT) |> reduceSeq(add)(l(0.0f))
  ))
  val dotSeqUnroll = fun(a => fun(b =>
    zip(a)(b) |> map(mulT) |> reduceSeqUnroll(add)(l(0.0f))
  ))

  val weights2d = l(ArrayData(
    Array(1, 2, 1, 2, 4, 2, 1, 2, 1).map(f => FloatData(f / 16.0f))))
  val weights1d = l(ArrayData(
    Array(1, 2, 1).map(f => FloatData(f / 4.0f))))

  val slide3x3 = map(slide(3)(1)) >> slide(3)(1) >> map(transpose)

  val highLevel =
    padClamp2D(1) >> slide3x3 >> map(map(fun(nbh => dot(weights2d)(join(nbh)))))

  val reference =
    padClamp2D(1) >> slide3x3 >> mapSeq(mapSeq(fun(nbh => dotSeq(weights2d)(join(nbh)))))

  val factorised =
    padClamp2D(1) >> slide3x3 >> mapSeq(mapSeq(map(dotSeq(weights1d)) >> dotSeq(weights1d)))

  val separated = {
    val horizontal = mapSeq(slide(3)(1) >> mapSeq(dotSeq(weights1d)))
    val vertical = slide(3)(1) >> mapSeq(transpose >> mapSeq(dotSeq(weights1d)))
    padClamp2D(1) >> vertical >> horizontal
  }

  val regrot =
    padClamp2D(1) >> slide(3)(1) >> mapSeq(transpose >>
      map(dotSeq(weights1d)) >>
      slideSeq(slideSeq.Values)(3)(1) >>
      map(dotSeq(weights1d))
    )

  val norm = normalize(betaReduction <+ etaReduction)

  val separateDot: Strategy = {
    case Apply(Apply(Apply(`reduce`, rf), init), Apply(Apply(`map`, mf), Apply(Apply(`zip`, w), Apply(`join`, nbh))))
    if rf == norm(add).get && init == l(0.0f) && mf == norm(mulT).get && w == weights2d
    =>
      Success(nbh |> map(dot(weights1d)) |> dot(weights1d))
    case _ => Failure(separateDot)
  }

  val separateDotT: Strategy = {
    case Apply(Apply(Apply(`reduce`, rf), init), Apply(Apply(`map`, mf), Apply(Apply(`zip`, w), Apply(`join`, nbh))))
      if rf == norm(add).get && init == l(0.0f) && mf == norm(mulT).get && w == weights2d
    =>
      Success(nbh |> transpose |> map(dot(weights1d)) |> dot(weights1d))
    case _ => Failure(separateDotT)
  }
}

class binomialFilter extends idealised.util.Tests {
  import binomialFilter._

  def s_eq(a: Expr, b: Expr): Unit = {
    if (!StructuralEquality(norm(a).get, norm(b).get)) {
      throw new Exception(s"expected structural equality:\n$a\n$b")
    }
  }

  test("rewrite to reference") {
    import strategies._
    import strategies.traversal._

    val s =
      oncetd(specialize.reduceSeq) `;`
        repeatNTimes(2)(oncetd(specialize.mapSeq))
    s_eq(s(highLevel).get, reference)
  }

  test("rewrite to factorised blur") {
    import strategies._
    import strategies.traversal._

    val s =
      reductionNormalform `;`
      oncetd(separateDot) `;`
        repeatNTimes(2)(oncetd(specialize.reduceSeq)) `;`
        repeatNTimes(2)(oncetd(specialize.mapSeq)) `;`
        reductionNormalform

    s_eq(s(highLevel).get,
      reductionNormalform(factorised).get)
  }

  test("rewrite to separated blur") {
    import strategies._
    import strategies.traversal._

    val * = map
    val T = transpose
    val P = padClamp2D(1)
    val Sh = slide(3)(1)
    val Sv = slide(3)(1)
    val Dh = dot(weights1d)
    val Dv = dot(weights1d)

    val steps = Seq[(Strategy, Expr)](
      (id,
        P >> *(Sh) >> Sv >> *(T) >> *(*(fun(nbh => dot(weights2d)(join(nbh)))))),
      (oncetd(separateDotT),
        P >> *(Sh) >> Sv >> *(T) >> *(*(T >> *(Dv) >> Dh))),
      (oncetd(`*f >> S -> S >> **f`),
        P >> Sv >> *(*(Sh)) >> *(T) >> *(*(T >> *(Dv) >> Dh))),
      (oncetd(mapFusion),
        P >> Sv >> *(*(Sh)) >> *(T >> *(T >> *(Dv) >> Dh))),
      (oncetd(mapFusion),
        P >> Sv >> *(*(Sh) >> T >> *(T >> *(Dv) >> Dh))),
      (oncetd(`*S >> T -> T >> S >> *T`),
        P >> Sv >> *(T >> Sh >> *(T) >> *(T >> *(Dv) >> Dh))),
      (oncetd(mapFusion),
        P >> Sv >> *(T >> Sh >> *(T >> T >> *(Dv) >> Dh))),
      (oncetd(`T >> T -> `),
        P >> Sv >> *(T >> Sh >> *(*(Dv) >> Dh))),
      (skip(1)(mapFirstFission),
        P >> Sv >> *(T >> Sh >> *(*(Dv)) >> *(Dh))),
      (oncetd(`S >> **f -> *f >> S`),
        P >> Sv >> *(T >> *(Dv) >> Sh >> *(Dh))),
      (oncetd(mapFirstFission),
        P >> Sv >> *(T) >> *(*(Dv) >> Sh >> *(Dh))),
      (oncetd(mapFirstFission),
        P >> Sv >> *(T) >> *(*(Dv)) >> *(Sh >> *(Dh))),
      (skip(1)(mapFusion),
        P >> Sv >> *(T >> *(Dv)) >> *(Sh >> *(Dh)))
    )

    val result = steps.foldLeft[Expr](highLevel)({ case (e, (s, expected)) =>
        val result = reductionNormalform(s(e).get).get
        s_eq(result, reductionNormalform(expected).get)
        result
    })

    val pick = repeatNTimes(2)(oncetd(specialize.reduceSeq)) `;`
      repeatNTimes(2)(oncetd(specialize.mapSeq)) `;`
      repeatNTimes(2)(skip(1)(specialize.mapSeq))
    s_eq(pick(result).get, reductionNormalform(separated).get)
  }

  test("rewrite to register rotation blur") {
    import strategies._
    import strategies.traversal._

    val * = map
    val T = transpose
    val P = padClamp2D(1)
    val Sh = slide(3)(1)
    val Sv = slide(3)(1)
    val Dh = dot(weights1d)
    val Dv = dot(weights1d)

    val steps = Seq[(Strategy, Expr)](
      (id,
        P >> *(Sh) >> Sv >> *(T) >> *(*(fun(nbh => dot(weights2d)(join(nbh)))))),
      (oncetd(separateDotT),
        P >> *(Sh) >> Sv >> *(T) >> *(*(T >> *(Dv) >> Dh))),
      (oncetd(`*f >> S -> S >> **f`),
        P >> Sv >> *(*(Sh)) >> *(T) >> *(*(T >> *(Dv) >> Dh))),
      (oncetd(mapFusion),
        P >> Sv >> *(*(Sh)) >> *(T >> *(T >> *(Dv) >> Dh))),
      (oncetd(mapFusion),
        P >> Sv >> *(*(Sh) >> T >> *(T >> *(Dv) >> Dh))),
      (oncetd(`*S >> T -> T >> S >> *T`),
        P >> Sv >> *(T >> Sh >> *(T) >> *(T >> *(Dv) >> Dh))),
      (oncetd(mapFusion),
        P >> Sv >> *(T >> Sh >> *(T >> T >> *(Dv) >> Dh))),
      (oncetd(`T >> T -> `),
        P >> Sv >> *(T >> Sh >> *(*(Dv) >> Dh))),
      (skip(1)(mapFirstFission),
        P >> Sv >> *(T >> Sh >> *(*(Dv)) >> *(Dh))),
      (oncetd(`S >> **f -> *f >> S`),
        P >> Sv >> *(T >> *(Dv) >> Sh >> *(Dh)))
    )

    val result = steps.foldLeft[Expr](highLevel)({ case (e, (s, expected)) =>
      val result = reductionNormalform(s(e).get).get
      s_eq(result, reductionNormalform(expected).get)
      result
    })

    val pick = repeatNTimes(2)(oncetd(specialize.reduceSeq)) `;`
      oncetd(specialize.slideSeq(slideSeq.Values)) `;`
      oncetd(specialize.mapSeq)
    s_eq(pick(result).get, reductionNormalform(regrot).get)
  }

  def program(name: String, e: Expr): C.Program = {
    val szr = lift.arithmetic.RangeAdd(3, lift.arithmetic.PosInf, 1)
    val typed = types.infer(
      nFun(szr, h => nFun(szr, w => fun(ArrayType(h, ArrayType(w, float)))(a => e(a))))
    )
    val phrase = idealised.DPIA.fromLift(typed)
    val program = C.ProgramGenerator.makeCode(phrase, name)
    SyntaxChecker(program.code)
    println(program.code)
    program
  }

  lazy val ref_prog = program("blur_ref", reference)
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
    if (delta < -0.001 || 0.001 < delta) {
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
    check_ref(factorised)
  }

  test("compile and compare separated blur to the reference") {
    check_ref(separated)
  }

  test("compile and compare register rotation blur to the reference") {
    check_ref(regrot)
  }

  test("register rotation blur with unroll should contain no modulo or division") {
    val code = program("blur",
      padClamp2D(1) >> slide(3)(1) >> mapSeq(transpose >>
        map(dotSeqUnroll(weights1d)) >>
        slideSeq(slideSeq.Values)(3)(1) >>
        map(dotSeqUnroll(weights1d))
      )).code
    " % ".r.findAllIn(code).length shouldBe 0
    " / ".r.findAllIn(code).length shouldBe 0
  }
}
