package elevate.core

import elevate.lift.rules.movement._
import elevate.core.strategies.traversal._
import elevate.core.strategies.basic._
import elevate.lift.strategies.normalForm._
import elevate.lift._
import lift.core.Expr
import lift.core.primitives._
import lift.core.DSL._

import scala.language.implicitConversions


class movement extends idealised.util.Tests {

  implicit def rewriteResultToExpr(r: RewriteResult): Expr = r.get
  val norm: Strategy = LCNF

  def testMultiple(list: List[Expr], gold: Expr) = {
    assert(list.forall(structEq(_, gold)))
  }

  // transpose

  test("**f >> T -> T >> **f") {
    val gold = λ(f => T >> **(f))

    testMultiple(
      List(
        norm(λ(f => *(λ(x => *(f)(x))) >> T)).get,
        λ(f => **(f) >> T)
      ).map((debug `;` oncetd(`**f >> T -> T >> **f`))(_).get), gold
    )
  }

  test("T >> **f -> **f >> T") {
    assert(structEq(
      oncetd(`T >> **f -> **f >> T`)(λ(f => T >> **(f))),
      λ(f => **(f) >> T))
    )
  }

  test("T >> ****f -> ****f >> T") {
    assert(structEq(
      oncetd(`T >> **f -> **f >> T`)(λ(f => T >> ****(f))),
      λ(f => ****(f) >> T))
    )
  }

  test("****f >> T -> T >> ****f") {
    assert(structEq(
      oncetd(`**f >> T -> T >> **f`)(λ(f => ****(f) >> T)),
      λ(f => T >> ****(f)))
    )
  }

  // split/slide

  test("S >> **f -> *f >> S") {
    assert(structEq(
      oncetd(`S >> **f -> *f >> S`)(λ(f => S >> **(f))),
      λ(f => *(f) >> S))
    )
  }

  test("*f >> S -> S >> **f") {
    assert(structEq(
      oncetd(`*f >> S -> S >> **f`)(λ(f => *(f) >> S)),
      λ(f => S >> **(f)))
    )
  }

  // join

  test("J >> *f -> **f >> J") {
    assert(structEq(
      oncetd(`J >> *f -> **f >> J`)(λ(f => J >> *(f))),
      λ(f => **(f) >> J)
    ))
  }

  test("**f >> J -> *f >> J") {
    assert(structEq(
      oncetd(`**f >> J -> J >> *f`)(λ(f => **(f) >> J)),
      λ(f => J >> *(f))
    ))
  }

  // special-cases

  test("T >> S -> *S >> T >> *T") {
    assert(structEq(
      oncetd(`T >> S -> *S >> T >> *T`)(T >> S),
      *(S) >> T >> *(T)
    ))
  }

  test("T >> *S -> S >> *T >> T") {
    assert(structEq(
      oncetd(`T >> *S -> S >> *T >> T`)(T >> *(S)),
      S >> *(T) >> T
    ))
  }

  test("*S >> T -> T >> S >> *T") {
    assert(structEq(
      oncetd(`*S >> T -> T >> S >> *T`)(*(S) >> T),
      T >> S >> *(T)
    ))
  }

  test("J >> T -> *T >> T >> *J") {
    assert(structEq(
      oncetd(`J >> T -> *T >> T >> *J`)(J >> T),
      *(T) >> T >> *(J)
    ))
  }

  test("T >> *J -> *T >> J >> T") {
    assert(structEq(
      oncetd(`T >> *J -> *T >> J >> T`)(T >> *(J)),
      *(T) >> J >> T
    ))
  }

  test("*T >> J -> T >> *J >> T") {
    assert(structEq(
      oncetd(`*T >> J -> T >> *J >> T`)(*(T) >> J),
      T >> *(J) >> T
    ))
  }

  test("*J >> T -> T >> *T >> J") {
    assert(structEq(
      oncetd(`*J >> T -> T >> *T >> J`)(*(J) >> T),
      T >> *(T) >> J
    ))
  }

  test("J >> J -> *J >> J") {
    assert(structEq(
      oncetd(`J >> J -> *J >> J`)(J >> J),
      *(J) >> J
    ))
  }

  test("*J >> J -> J >> J") {
    assert(structEq(
      oncetd(`*J >> J -> J >> J`)(*(J) >> J),
      J >> J
    ))
  }

  test("slideOverSplit") {
    assert(structEq(
      oncetd(slideBeforeSplit)(slide(3)(1) >> split(16)),
      slide(16+3-1)(16) >> map(slide(3)(1))
    ))
  }
}
