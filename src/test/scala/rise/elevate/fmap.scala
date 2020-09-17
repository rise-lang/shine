package rise.elevate

import elevate.core.strategies.traversal._
import elevate.core.{Failure, Success}
import rise.elevate.util._
import rise.elevate.rules.movement._
import rise.elevate.rules.traversal._
import rise.elevate.rules.traversal.default._

class fmap extends elevate.test_util.Tests {

  val fmapRNF = rise.elevate.strategies.traversal.fmapRNF(RiseTraversable)

  val mapped = rise.elevate.strategies.traversal.mapped(RiseTraversable)

  test("fmap basic level0") {
    assert(betaEtaEquals(
      one(one(`**f >> T -> T >> **f`())).apply(λ(f => **(f) >> T)).get,
      λ(f => T >> **(f)))
    )
  }

  test("fmap basic level1") {
    assert(betaEtaEquals(
      one(one(fmapRNF(`**f >> T -> T >> **f`()(RiseTraversable)))).apply(λ(f => ***(f) >> *(T))).get,
      λ(f => *(T) >> ***(f)))
    )
  }

  test("fmap basic level2") {
    assert(betaEtaEquals(
      one(one(fmapRNF(fmapRNF(`**f >> T -> T >> **f`()(RiseTraversable))))).apply(λ(f => ****(f) >> **(T))).get,
      λ(f => **(T) >> ****(f)))
    )
  }

  test("fmap basic level3") {
    assert(betaEtaEquals(
      one(one(fmapRNF(fmapRNF(fmapRNF(`**f >> T -> T >> **f`()(RiseTraversable)))))).apply(λ(f => *****(f) >> ***(T))).get,
      λ(f => ***(T) >> *****(f)))
    )
  }

  test("fmap basic level4") {
    assert(betaEtaEquals(
      one(one(fmapRNF(fmapRNF(fmapRNF(fmapRNF(`**f >> T -> T >> **f`()(RiseTraversable))))))).apply(λ(f => ******(f) >> ****(T))).get,
      λ(f => ****(T) >> ******(f)))
    )
  }

  test("fmap basic level4 alternative") {
    assert(betaEtaEquals(
      one(one(mapped(`**f >> T -> T >> **f`()(RiseTraversable)))).apply(λ(f => ******(f) >> ****(T))).get,
      λ(f => ****(T) >> ******(f)))
    )
  }

  test("fmap should fail") {
    assert(
      body(one(mapped(`**f >> T -> T >> **f`()(RiseTraversable))))(λ(f => *****(f) >> ****(T))) match {
        case Failure(_) => true
        case Success(_) => false
      }
    )
  }

  test("fmap advanced + lift specific traversals") {
    // mapped pattern before
    testMultiple(
      List(
        body(body(mapped(`**f >> T -> T >> **f`()(RiseTraversable))))(λ(f => *(S) >> ***(f) >> *(T))).get,
        body(body(fmapRNF(`**f >> T -> T >> **f`()(RiseTraversable))))(λ(f => *(S) >> ***(f) >> *(T))).get
      ), λ(f => *(S) >> *(T) >> ***(f))
    )

    // mapped pattern after
    // we got to jump "over" this pattern before the rule is applicable
    testMultiple(
      List(
        body(body(argument(mapped(`**f >> T -> T >> **f`()(RiseTraversable)))))(λ(f => ***(f) >> *(T) >> *(S))).get,
        body(body(argument(fmapRNF(`**f >> T -> T >> **f`()(RiseTraversable)))))(λ(f => ***(f) >> *(T) >> *(S))).get
      ), λ(f => *(T) >> ***(f) >> *(S))
    )

    // ...or we could simply "find" the place automatically
    testMultiple(
      List(
        topDown(mapped(`**f >> T -> T >> **f`()(RiseTraversable))).apply(λ(f => ***(f) >> *(T) >> *(S))).get,
        topDown(fmapRNF(`**f >> T -> T >> **f`()(RiseTraversable))).apply(λ(f => ***(f) >> *(T) >> *(S))).get
      ), λ(f => *(T) >> ***(f) >> *(S))
    )

    // testing mapped specific behaviour below: mapped can be used without needing to know
    // how many times I need to nest `fmap` to get the same behaviour

    testMultiple(
      List(
        body(body(mapped(`**f >> T -> T >> **f`()(RiseTraversable))))(λ(f => *(S) >> ****(f) >> **(T))).get,
        body(body(fmapRNF(fmapRNF(`**f >> T -> T >> **f`()(RiseTraversable)))))(λ(f => *(S) >> ****(f) >> **(T))).get
      ), λ(f => *(S) >> **(T) >> ****(f))
    )

    testMultiple(
      List(
        body(body(argument(mapped(`**f >> T -> T >> **f`()(RiseTraversable)))))(λ(f => ****(f) >> **(T) >> *(S))).get,
        body(body(argument(fmapRNF(fmapRNF(`**f >> T -> T >> **f`()(RiseTraversable))))))(λ(f => ****(f) >> **(T) >> *(S))).get
      ), λ(f => **(T) >> ****(f) >> *(S))
    )

    testMultiple(
      List(
        topDown(mapped(`**f >> T -> T >> **f`()(RiseTraversable))).apply(λ(f => ****(f) >> **(T) >> *(S))).get,
        topDown(fmapRNF(fmapRNF(`**f >> T -> T >> **f`()(RiseTraversable)))).apply(λ(f => ****(f) >> **(T) >> *(S))).get
      ), λ(f => **(T) >> ****(f) >> *(S))
    )
  }
}
