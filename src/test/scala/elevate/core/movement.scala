package elevate.core

import elevate.core.rules._
import elevate.core.rules.movement._
import elevate.core.strategies._
import elevate.core.strategies.traversal._
import lift.core.DSL._
import lift.core._
import lift.core.primitives._

class movement extends idealised.util.Tests {

  val norm: Strategy = normalize(betaReduction <+ etaReduction)
  def eq(a: Expr, b: Expr): Boolean = StructuralEquality(norm(a), norm(b))

  // notation
  val T: Expr = transpose
  val S: Expr = split(4)//slide(3)(1)
  def *(x: Expr): Expr = map(x)
  def **(x: Expr): Expr = map(map(x))
  def λ(f: Identifier => Expr): Expr = fun(f)

  /// transpose ////////////////////////////////////////////////////////////////////////////////////////////////////////

  test("**f >> T -> T >> **f") {
    val gold = λ(f => T >> **(f))

    assert(
      List(
        norm(λ(f => *(λ(x => *(f)(x))) >> T)),
        λ(f => **(f) >> T)
      ).forall(expr =>
        eq(oncetd(`**f >> T -> T >> **f`)(expr), gold))
    )
  }

  test("T >> **f -> **f >> T") {
    assert(eq(
      oncetd(`T >> **f -> **f >> T`)(λ(f => T >> **(f))),
      λ(f => **(f) >> T))
    )
  }

  /// split/slide //////////////////////////////////////////////////////////////////////////////////////////////////////

  test("S >> **f -> *f >> S") {
    assert(eq(
      oncetd(`S >> **f -> *f >> S`)(λ(f => S >> **(f))),
      λ(f => *(f) >> S))
    )
  }

  test("*f >> S -> S >> **f") {
    assert(eq(
      oncetd(`*f >> S -> S >> **f`)(λ(f => *(f) >> S)),
      λ(f => S >> **(f)))
    )
  }

  /// join /////////////////////////////////////////////////////////////////////////////////////////////////////////////
}
