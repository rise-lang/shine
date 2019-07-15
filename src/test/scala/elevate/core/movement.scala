package elevate.core

import elevate.core.rules._
import elevate.core.rules.movement._
import elevate.core.strategies._
import elevate.core.strategies.traversal._
import elevate.core.strategies.liftTraversal._
import elevate.core.strategies.normalForm._
import lift.core.{Expr, Identifier, StructuralEquality}
import lift.core.primitives._
import lift.core.DSL._
import scala.language.implicitConversions

class movement extends idealised.util.Tests {

  implicit def rewriteResultToExpr(r: RewriteResult): Expr = r.get

  val norm: Strategy = normalize(betaReduction <+ etaReduction)
  def eq(a: Expr, b: Expr): Boolean = StructuralEquality(norm(a), norm(b))

  // notation
  def T: Expr = transpose
  def S: Expr = split(4)//slide(3)(1)
  def J: Expr = join
  def *(x: Expr): Expr = map(x)
  def **(x: Expr): Expr = map(map(x))
  def ***(x: Expr): Expr = map(map(map(x)))
  def ****(x: Expr): Expr = map(map(map(map(x))))
  def *****(x: Expr): Expr = map(map(map(map(map(x)))))
  def ******(x: Expr): Expr = map(map(map(map(map(map(x))))))
  def λ(f: Identifier => Expr): Expr = fun(f)

  def testMultiple(list: List[Expr], gold: Expr) = {
    assert(list.forall(eq(_, gold)))
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
    assert(eq(
      one(one(`**f >> T -> T >> **f`))(λ(f => **(f) >> T)),
      λ(f => T >> **(f)))
    )

    // level 1
    assert(eq(
      one(one(fmap(`**f >> T -> T >> **f`)))(λ(f => ***(f) >> *(T))),
      λ(f => *(T) >> ***(f)))
    )

    // level 2
    assert(eq(
      one(one(fmap(fmap(`**f >> T -> T >> **f`))))(λ(f => ****(f) >> **(T))),
      λ(f => **(T) >> ****(f)))
    )

    // level 3
    assert(eq(
      one(one(fmap(fmap(fmap(`**f >> T -> T >> **f`)))))(λ(f => *****(f) >> ***(T))),
      λ(f => ***(T) >> *****(f)))
    )

    // level 4
    assert(eq(
      one(one(fmap(fmap(fmap(fmap(`**f >> T -> T >> **f`))))))(λ(f => ******(f) >> ****(T))),
      λ(f => ****(T) >> ******(f)))
    )

    // level 4 alternative
    assert(eq(
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
        body(body(fmap(`**f >> T -> T >> **f`)))(λ(f => *(S) >> ***(f) >> *(T)))
      ), λ(f => *(S) >> *(T) >> ***(f))
    )

    // mapped pattern after
    // we got to jump "over" this pattern before the rule is applicable
    testMultiple(
      List(
        body(body(argument(mapped(`**f >> T -> T >> **f`))))(λ(f => ***(f) >> *(T) >> *(S))),
        body(body(argument(fmap(`**f >> T -> T >> **f`))))(λ(f => ***(f) >> *(T) >> *(S)))
      ), λ(f => *(T) >> ***(f) >> *(S))
    )

    // ...or we could simply "find" the place automatically
    testMultiple(
      List(
        oncetd(mapped(`**f >> T -> T >> **f`))(λ(f => ***(f) >> *(T) >> *(S))),
        oncetd(fmap(`**f >> T -> T >> **f`))(λ(f => ***(f) >> *(T) >> *(S)))
      ), λ(f => *(T) >> ***(f) >> *(S))
    )

    // testing mapped specific behaviour below: mapped can be used without needing to know
    // how many times I need to nest `fmap` to get the same behaviour

    testMultiple(
      List(
        body(body(mapped(`**f >> T -> T >> **f`)))(λ(f => *(S) >> ****(f) >> **(T))),
        body(body(fmap(fmap(`**f >> T -> T >> **f`))))(λ(f => *(S) >> ****(f) >> **(T)))
      ), λ(f => *(S) >> **(T) >> ****(f))
    )

    testMultiple(
      List(
        body(body(argument(mapped(`**f >> T -> T >> **f`))))(λ(f => ****(f) >> **(T) >> *(S))),
        body(body(argument(fmap(fmap(`**f >> T -> T >> **f`)))))(λ(f => ****(f) >> **(T) >> *(S)))
      ), λ(f => **(T) >> ****(f) >> *(S))
    )

    testMultiple(
      List(
        oncetd(mapped(`**f >> T -> T >> **f`))(λ(f => ****(f) >> **(T) >> *(S))),
        oncetd(fmap(fmap(`**f >> T -> T >> **f`)))(λ(f => ****(f) >> **(T) >> *(S)))
      ), λ(f => **(T) >> ****(f) >> *(S))
    )
  }

  test("T >> **f -> **f >> T") {
    assert(eq(
      oncetd(`T >> **f -> **f >> T`)(λ(f => T >> **(f))),
      λ(f => **(f) >> T))
    )
  }

  // split/slide

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

  // join

  test("J >> *f -> **f >> J") {
    assert(eq(
      oncetd(`J >> *f -> **f >> J`)(λ(f => J >> *(f))),
      λ(f => **(f) >> J)
    ))
  }

  test("**f >> J -> *f >> J") {
    assert(eq(
      oncetd(`**f >> J -> J >> *f`)(λ(f => **(f) >> J)),
      λ(f => J >> *(f))
    ))
  }

  // special-cases

  test("T >> S -> *S >> T >> *T") {
    assert(eq(
      oncetd(`T >> S -> *S >> T >> *T`)(T >> S),
      *(S) >> T >> *(T)
    ))
  }

  test("T >> *S -> S >> *T >> T") {
    assert(eq(
      oncetd(`T >> *S -> S >> *T >> T`)(T >> *(S)),
      S >> *(T) >> T
    ))
  }

  test("*S >> T -> T >> S >> *T") {
    assert(eq(
      oncetd(`*S >> T -> T >> S >> *T`)(*(S) >> T),
      T >> S >> *(T)
    ))
  }

  test("J >> T -> *T >> T >> *J") {
    assert(eq(
      oncetd(`J >> T -> *T >> T >> *J`)(J >> T),
      *(T) >> T >> *(J)
    ))
  }

  test("T >> *J -> *T >> J >> T") {
    assert(eq(
      oncetd(`T >> *J -> *T >> J >> T`)(T >> *(J)),
      *(T) >> J >> T
    ))
  }

  test("*T >> J -> T >> *J >> T") {
    assert(eq(
      oncetd(`*T >> J -> T >> *J >> T`)(*(T) >> J),
      T >> *(J) >> T
    ))
  }

  test("*J >> T -> T >> *T >> J") {
    assert(eq(
      oncetd(`*J >> T -> T >> *T >> J`)(*(J) >> T),
      T >> *(T) >> J
    ))
  }

  test("J >> J -> *J >> J") {
    assert(eq(
      oncetd(`J >> J -> *J >> J`)(J >> J),
      *(J) >> J
    ))
  }

  test("*J >> J -> J >> J") {
    assert(eq(
      oncetd(`*J >> J -> J >> J`)(*(J) >> J),
      J >> J
    ))
  }

  test("slideOverSplit") {
    assert(eq(
      oncetd(slideBeforeSplit)(slide(3)(1) >> split(16)),
      slide(16+3-1)(16) >> map(slide(3)(1))
    ))
  }
}
