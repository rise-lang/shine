package elevate.core

import elevate.lift.rules.movement._
import elevate.core.strategies.traversal._
import elevate.lift.strategies.traversal._
import elevate.lift._
import lift.core.{Expr, Identifier, StructuralEquality}
import lift.core.DSL._

import scala.language.implicitConversions


class fmap extends idealised.util.Tests {

  implicit def rewriteResultToExpr(r: RewriteResult): Expr = r.get
  def testMultiple(list: List[Expr], gold: Expr) = {
    assert(list.forall(structEq(_, gold)))
  }

  test("fmap basic level0") {
    assert(structEq(
      one(one(`**f >> T -> T >> **f`))(λ(f => **(f) >> T)),
      λ(f => T >> **(f)))
    )
  }

  test("fmap basic level1") {
    assert(structEq(
      one(one(fmapRNF(`**f >> T -> T >> **f`)))(λ(f => ***(f) >> *(T))),
      λ(f => *(T) >> ***(f)))
    )
  }

  test("fmap basic level2") {
    assert(structEq(
      one(one(fmapRNF(fmapRNF(`**f >> T -> T >> **f`))))(λ(f => ****(f) >> **(T))),
      λ(f => **(T) >> ****(f)))
    )
  }

  test("fmap basic level3") {
    assert(structEq(
      one(one(fmapRNF(fmapRNF(fmapRNF(`**f >> T -> T >> **f`)))))(λ(f => *****(f) >> ***(T))),
      λ(f => ***(T) >> *****(f)))
    )
  }

  test("fmap basic level4") {
    assert(structEq(
      one(one(fmapRNF(fmapRNF(fmapRNF(fmapRNF(`**f >> T -> T >> **f`))))))(λ(f => ******(f) >> ****(T))),
      λ(f => ****(T) >> ******(f)))
    )
  }

  test("fmap basic level4 alternative") {
    assert(structEq(
      one(one(mapped(`**f >> T -> T >> **f`)))(λ(f => ******(f) >> ****(T))),
      λ(f => ****(T) >> ******(f)))
    )
  }

  test("fmap should fail") {
    assert(
      body(one(mapped(`**f >> T -> T >> **f`)))(λ(f => *****(f) >> ****(T))) match {
        case Failure(_) => true
        case Success(_) => false
      }
    )
  }

  test("fmap advanced + lift specific traversals") {
    // mapped pattern before
    testMultiple(
      List(
        body(body(mapped(`**f >> T -> T >> **f`)))(λ(f => *(S) >> ***(f) >> *(T))),
        body(body(fmapRNF(`**f >> T -> T >> **f`)))(λ(f => *(S) >> ***(f) >> *(T)))
      ), λ(f => *(S) >> *(T) >> ***(f))
    )

    // mapped pattern after
    // we got to jump "over" this pattern before the rule is applicable
    testMultiple(
      List(
        body(body(argument(mapped(`**f >> T -> T >> **f`))))(λ(f => ***(f) >> *(T) >> *(S))),
        body(body(argument(fmapRNF(`**f >> T -> T >> **f`))))(λ(f => ***(f) >> *(T) >> *(S)))
      ), λ(f => *(T) >> ***(f) >> *(S))
    )

    // ...or we could simply "find" the place automatically
    testMultiple(
      List(
        oncetd(mapped(`**f >> T -> T >> **f`))(λ(f => ***(f) >> *(T) >> *(S))),
        oncetd(fmapRNF(`**f >> T -> T >> **f`))(λ(f => ***(f) >> *(T) >> *(S)))
      ), λ(f => *(T) >> ***(f) >> *(S))
    )

    // testing mapped specific behaviour below: mapped can be used without needing to know
    // how many times I need to nest `fmap` to get the same behaviour

    testMultiple(
      List(
        body(body(mapped(`**f >> T -> T >> **f`)))(λ(f => *(S) >> ****(f) >> **(T))),
        body(body(fmapRNF(fmapRNF(`**f >> T -> T >> **f`))))(λ(f => *(S) >> ****(f) >> **(T)))
      ), λ(f => *(S) >> **(T) >> ****(f))
    )

    testMultiple(
      List(
        body(body(argument(mapped(`**f >> T -> T >> **f`))))(λ(f => ****(f) >> **(T) >> *(S))),
        body(body(argument(fmapRNF(fmapRNF(`**f >> T -> T >> **f`)))))(λ(f => ****(f) >> **(T) >> *(S)))
      ), λ(f => **(T) >> ****(f) >> *(S))
    )

    testMultiple(
      List(
        oncetd(mapped(`**f >> T -> T >> **f`))(λ(f => ****(f) >> **(T) >> *(S))),
        oncetd(fmapRNF(fmapRNF(`**f >> T -> T >> **f`)))(λ(f => ****(f) >> **(T) >> *(S)))
      ), λ(f => **(T) >> ****(f) >> *(S))
    )
  }
}
