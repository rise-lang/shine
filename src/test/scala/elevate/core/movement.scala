package elevate.core

import elevate.lift.rules._
import elevate.lift.rules.movement._
import elevate.core.strategies._
import elevate.core.strategies.traversal._
import elevate.lift.strategies.traversal._
import elevate.lift.strategies.normalForm._
import elevate.lift._
import lift.core.{Expr, Identifier, StructuralEquality}
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
      ).map(oncetd(`**f >> T -> T >> **f`)(_).get), gold
    )
  }

  test("fmap basic") {
    // level 0
    assert(structEq(
      one(one(`**f >> T -> T >> **f`))(λ(f => **(f) >> T)),
      λ(f => T >> **(f)))
    )

    // level 1
    assert(structEq(
      one(one(fmapRNF(`**f >> T -> T >> **f`)))(λ(f => ***(f) >> *(T))),
      λ(f => *(T) >> ***(f)))
    )

    // level 2
    assert(structEq(
      one(one(fmapRNF(fmapRNF(`**f >> T -> T >> **f`))))(λ(f => ****(f) >> **(T))),
      λ(f => **(T) >> ****(f)))
    )

    // level 3
    assert(structEq(
      one(one(fmapRNF(fmapRNF(fmapRNF(`**f >> T -> T >> **f`)))))(λ(f => *****(f) >> ***(T))),
      λ(f => ***(T) >> *****(f)))
    )

    // level 4
    assert(structEq(
      one(one(fmapRNF(fmapRNF(fmapRNF(fmapRNF(`**f >> T -> T >> **f`))))))(λ(f => ******(f) >> ****(T))),
      λ(f => ****(T) >> ******(f)))
    )

    // level 4 alternative
    assert(structEq(
      one(one(mapped(`**f >> T -> T >> **f`)))(λ(f => ******(f) >> ****(T))),
      λ(f => ****(T) >> ******(f)))
    )

    // should fail
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
