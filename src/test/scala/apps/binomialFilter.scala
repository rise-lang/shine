package apps

import lift.core._
import lift.core.primitives._
import lift.core.semantics._
import lift.core.DSL._
import elevate.core._
import strategies._
import rules._
import rules.algorithmic._
import strategies.algorithmic._

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

  // TODO: pad
  // TODO: registers/loop unrolling, vectorisation
  // TODO: rewriting

  val weights2d = Literal(ArrayData(
    Array(1, 2, 1, 2, 4, 2, 1, 2, 1).map(f => FloatData(f / 16.0f))))
  val weights1d = Literal(ArrayData(Seq(FloatData(1.0f))))//Array(1, 2, 1).map(f => FloatData(f / 4.0f))))

  val slide3x3 = map(slide(3)(1)) >> slide(3)(1) >> map(transpose)

  val highLevel =
    slide3x3 >> map(map(fun(nbh => dot(weights2d)(join(nbh)))))

  val reference =
    slide3x3 >> mapSeq(mapSeq(fun(nbh => dotSeq(weights2d)(join(nbh)))))

  val factorised =
    slide3x3 >> mapSeq(mapSeq(map(dotSeq(weights1d)) >> dotSeq(weights1d)))

  val separated = {
    val horizontal = mapSeq(slide(3)(1) >> mapSeq(dotSeq(weights1d)))
    val vertical = slide(3)(1) >> mapSeq(transpose >> mapSeq(dotSeq(weights1d)))
    vertical >> horizontal
  }

  val regrot_blur =
    slide(3)(1) >> mapSeq(transpose >>
      map(dotSeq(weights1d)) >> slideSeq(3)(1) >> map(dotSeq(weights1d))
    )

  val norm = normalize(betaReduction +> etaReduction)

  val separateDot: Strategy = {
    case Apply(Apply(Apply(`reduce`, rf), init), Apply(Apply(`map`, mf), Apply(Apply(`zip`, w), Apply(`join`, nbh))))
    if rf == norm(add) && init == l(0.0f) && mf == norm(mulT) && w == weights2d
    =>
      nbh |> map(dot(weights1d)) |> dot(weights1d)
  }

  val separateDotT: Strategy = {
    case Apply(Apply(Apply(`reduce`, rf), init), Apply(Apply(`map`, mf), Apply(Apply(`zip`, w), Apply(`join`, nbh))))
      if rf == norm(add) && init == l(0.0f) && mf == norm(mulT) && w == weights2d
    =>
      nbh |> transpose |> map(dot(weights1d)) |> dot(weights1d)
  }
}

class binomialFilter extends idealised.util.Tests {
  import binomialFilter._

  def assertExprEq(a: Expr, b: Expr): Unit = {
    println(s"$a")
    println(s"~ $b")
    if (!StructuralEquality(a, b)) {
      throw new Exception(s"expected structural equality")
    }
  }

  test("rewrite to reference") {
    val s =
      applyDFS(specialize.reduceSeq) `;`
        repeatNTimes(2)(applyDFS(specialize.mapSeq))
    assertExprEq(s(highLevel), reference)
  }

  test("rewrite to factorised blur") {
    val s =
      norm `;`
      applyDFS(separateDot) `;`
        repeatNTimes(2)(applyDFS(specialize.reduceSeq)) `;`
        repeatNTimes(2)(applyDFS(specialize.mapSeq)) `;`
        norm

    assertExprEq(s(highLevel), norm(factorised))
  }

  test("rewrite to separated blur") {
    val * = map
    val T = transpose
    val Sh = slide(3)(1)
    val Sv = slide(3)(1)
    val Dh = dot(weights1d)
    val Dv = dot(weights1d)

    val steps = Seq[(Strategy, Expr)](
      (id,
        *(Sh) >> Sv >> *(T) >> *(*(fun(nbh => dot(weights2d)(join(nbh)))))),
      (applyDFS(separateDotT),
        *(Sh) >> Sv >> *(T) >> *(*(T >> *(Dv) >> Dh))),
      (applyDFS(`*f >> S -> S >> **f`),
        Sv >> *(*(Sh)) >> *(T) >> *(*(T >> *(Dv) >> Dh))),
      (applyDFS(mapFusion),
        Sv >> *(*(Sh)) >> *(T >> *(T >> *(Dv) >> Dh))),
      (applyDFS(mapFusion),
        Sv >> *(*(Sh) >> T >> *(T >> *(Dv) >> Dh))),
      (applyDFS(`*S >> T -> T >> S >> *T`),
        Sv >> *(T >> Sh >> *(T) >> *(T >> *(Dv) >> Dh))),
      (applyDFS(mapFusion),
        Sv >> *(T >> Sh >> *(T >> T >> *(Dv) >> Dh))),
      (applyDFS(`T >> T -> `),
        Sv >> *(T >> Sh >> *(*(Dv) >> Dh))),
      (applyDFSSkip(1)(mapInnerFission),
        Sv >> *(T >> Sh >> *(*(Dv)) >> *(Dh))),
      (applyDFS(`S >> **f -> *f >> S`),
        Sv >> *(T >> *(Dv) >> Sh >> *(Dh))),
      (applyDFS(mapInnerFission),
        Sv >> *(T) >> *(*(Dv) >> Sh >> *(Dh))),
      (applyDFS(mapInnerFission),
        Sv >> *(T) >> *(*(Dv)) >> *(Sh >> *(Dh))),
      (applyDFSSkip(1)(mapFusion),
        Sv >> *(T >> *(Dv)) >> *(Sh >> *(Dh)))
    )

    val result = steps.foldLeft[Expr](highLevel)({ case (e, (s, expected)) =>
        val result = norm(s(e))
        assertExprEq(result, norm(expected))
        result
    })

    val pick = repeatNTimes(2)(applyDFS(specialize.reduceSeq)) `;`
      repeatNTimes(2)(applyDFS(specialize.mapSeq)) `;`
      repeatNTimes(2)(applyDFSSkip(1)(specialize.mapSeq))
    assertExprEq(pick(result), norm(separated))
  }
}
