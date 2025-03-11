package rise.elevate

import elevate.core.strategies.predicate.rewriteResultToBoolean
import elevate.core.strategies.traversal._
import rise.elevate.util._
import rise.core.DSL._
import rise.core.primitives._
import rise.core._
import rise.core.types.Nat
import rise.core.types.DataType._
import Type._
import rise.elevate.rules.movement._
import rise.elevate.rules.traversal.default._

class movement extends test_util.Tests {

  // transpose

  val BENF = rise.elevate.strategies.normalForm.BENF()(RiseTraversable)
  val DFNF = rise.elevate.strategies.normalForm.DFNF()(RiseTraversable)

  def betaEtaEquals(a: Rise, b: Rise): Boolean = {
    val na = BENF(a).get
    val nb = BENF(b).get
    // TODO: investigate why type erasure is required here
    // https://github.com/rise-lang/shine/issues/86
    val uab: Rise = eraseType(na) :: nb.t
    makeClosed(uab) =~= makeClosed(nb)
  }

  test("**f >> T -> T >> **f") {
    val gold = λ(f => T >> **(f))

    testMultiple(
      List(
        DFNF(λ(f => *(λ(x => *(f)(x))) >> T)).get,
        toExpr(λ(f => **(f) >> T))
      ).map(topDown(`**f >> T -> T >> **f`()(RiseTraversable)).apply(_).get), gold
    )
  }

  // FIXME: Not work because mapMapFBeforeTranspose is not general enough
  ignore("**f >> T - zip constraint") {
    // val test = λ(i => λ(f => (T o ***(f)) $ zip(i,i)))

    val backward: Expr =
      depFun((m: Nat, n: Nat, k: Nat) =>
        fun((m`.`k`.`f32) ->: (k`.`n`.`f32) ->: (m`.`n`.`f32) ->: f32 ->: f32 ->: (n`.`m`.`f32))
        ((a, b, c, alpha, beta) =>
          (transpose o map(fun(ac =>
            map(fun(bc =>
              (fun(x => (x * alpha) + beta * bc.`2`) o
                reduceSeq(fun((acc, y) => acc + (y.`1` * y.`2`)))(lf32(0.0f))) $
                zip(ac.`1`)(bc.`1`))) $
              zip(transpose(b))(ac.`2`)))) $
            zip(a)(c)
        )
      )

    assert(topDown(mapMapFBeforeTranspose()).apply(backward))
  }

  test("T >> **f -> **f >> T") {
    assert(betaEtaEquals(
      topDown(`T >> **f -> **f >> T`).apply(λ(f => T >> **(f))).get,
      λ(f => **(f) >> T))
    )
  }

  test("T >> ****f -> ****f >> T") {
    assert(betaEtaEquals(
      topDown(`T >> **f -> **f >> T`).apply(λ(f => T >> ****(f))).get,
      λ(f => ****(f) >> T))
    )
  }

  test("****f >> T -> T >> ****f") {
    assert(betaEtaEquals(
      topDown(`**f >> T -> T >> **f`()(RiseTraversable)).apply(λ(f => ****(f) >> T)).get,
      λ(f => T >> ****(f)))
    )
  }

  // split/slide

  test("S >> **f -> *f >> S") {
    assert(betaEtaEquals(
      topDown(`S >> **f -> *f >> S`).apply(λ(f => S >> **(f))).get,
      λ(f => *(f) >> S))
    )
  }

  test("*f >> S -> S >> **f") {
    assert(betaEtaEquals(
      topDown(splitBeforeMap).apply(λ(f => *(f) >> S)).get,
      λ(f => S >> **(f)))
    )
  }

  // join

  test("J >> *f -> **f >> J") {
    assert(betaEtaEquals(
      topDown(`J >> *f -> **f >> J`).apply(λ(f => J >> *(f))).get,
      λ(f => **(f) >> J)
    ))
  }

  test("**f >> J -> *f >> J") {
    assert(betaEtaEquals(
      topDown(`**f >> J -> J >> *f`).apply(λ(f => **(f) >> J)).get,
      λ(f => J >> *(f))
    ))
  }

  // special-cases

  test("T >> S -> *S >> T >> *T") {
    assert(betaEtaEquals(
      topDown(`T >> S -> *S >> T >> *T`).apply(T >> S).get,
      *(S) >> T >> *(T)
    ))
  }

  test("T >> *S -> S >> *T >> T") {
    assert(betaEtaEquals(
      topDown(`T >> *S -> S >> *T >> T`).apply(T >> *(S)).get,
      S >> *(T) >> T
    ))
  }

  test("*S >> T -> T >> S >> *T") {
    assert(betaEtaEquals(
      topDown(`*S >> T -> T >> S >> *T`).apply(*(S) >> T).get,
      T >> S >> *(T)
    ))
  }

  test("J >> T -> *T >> T >> *J") {
    assert(betaEtaEquals(
      topDown(`J >> T -> *T >> T >> *J`).apply(J >> T).get,
      *(T) >> T >> *(J)
    ))
  }

  test("T >> *J -> *T >> J >> T") {
    assert(betaEtaEquals(
      topDown(`T >> *J -> *T >> J >> T`).apply(T >> *(J)).get,
      *(T) >> J >> T
    ))
  }

  test("*T >> J -> T >> *J >> T") {
    assert(betaEtaEquals(
      topDown(`*T >> J -> T >> *J >> T`).apply(*(T) >> J).get,
      T >> *(J) >> T
    ))
  }

  test("*J >> T -> T >> *T >> J") {
    assert(betaEtaEquals(
      topDown(`*J >> T -> T >> *T >> J`).apply(*(J) >> T).get,
      T >> *(T) >> J
    ))
  }

  test("J >> J -> *J >> J") {
    assert(betaEtaEquals(
      topDown(`J >> J -> *J >> J`).apply(J >> J).get,
      *(J) >> J
    ))
  }

  test("*J >> J -> J >> J") {
    assert(betaEtaEquals(
      topDown(`*J >> J -> J >> J`).apply(*(J) >> J).get,
      J >> J
    ))
  }

  test("slideOverSplit") {
    assert(betaEtaEquals(
      topDown(slideBeforeSplit).apply(slide(3)(1) >> split(16)).get,
      slide(16+3-1)(16) >> map(slide(3)(1))
    ))
  }
}
