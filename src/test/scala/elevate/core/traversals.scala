package elevate.core

import elevate.lift.rules._
import elevate.util._
import elevate.lift.strategies.normalForm._
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.lift.strategies.traversal._
import elevate.meta.rules.fission._
import elevate.lift.rules.movement._
import elevate.lift.strategies.tiling._
import elevate.lift.strategies.util._
import elevate.lift._
import elevate.lift.rules
import elevate.lift.rules.algorithmic._
import elevate.lift.strategies.predicate.isLambda
import elevate.lift.strategies.traversal.{body, function, inBody}
import elevate.meta.rules.fission.bodyFission
import lift.core.DSL._
import lift.core._
import lift.core.primitives._
import lift.core.types.NatKind


class traversals extends test_util.Tests {

  test("rewrite simple elevate strategy") {
    val expr = fun(f => fun(g => map(f) >> map(g)))
    val strategy = body(body(body(mapFusion `;` function(mapLastFission))))

    val metaStrategy = inBody(inBody(bodyFission))(strategy)
    val newStrategy = metaStrategy.get
    assert(strategy != newStrategy)
    assert(newStrategy(expr).get == strategy(expr).get)
  }

  test("simplification") {
    val input2D = λ(i => λ(f => **!(f) $ i))
    val orig = body(body(tileND(2)(tileSize)))
    println(orig)

    val oldTiling = body(body(
      function(argumentOf(map,body(function(splitJoin(4)) `;` LCNF `;` RNF))) `;`
        function(splitJoin(4)) `;`
        LCNF `;` RNF `;` LCNF `;` RNF `;` LCNF `;`
        argument(argument(function(argumentOf(map,body(idAfter `;` createTransposePair `;` LCNF `;` argument(mapMapFBeforeTranspose)))) `;` LCNF `;` RNF)) `;`
        LCNF `;` RNF `;` LCNF `;` RNF `;` RNF
    ))

    val simplified = body(body(
      function(argumentOf(map,body(function(splitJoin(4))))) `;`
        function(splitJoin(4)) `;`
        RNF `;` LCNF `;`
        argument(argument(function(argumentOf(map,body(idAfter `;` createTransposePair `;` LCNF `;` argument(mapMapFBeforeTranspose)))) `;` RNF))))

    val normalized = FNF(simplified).get
    println(normalized)
    val normalizedModified = body(body(function(argumentOf(map,body(function(splitJoin(4))))))) `;`
    inferType `;`
      body(body(function(splitJoin(4)))) `;`
      inferType `;`
      body(body(RNF)) `;`
      inferType `;`
      body(body(LCNF)) `;`
      inferType `;`
      body(body(argument(argument(function(argumentOf(map,body(idAfter))))))) `;`
      inferType `;`
      body(body(argument(argument(function(argumentOf(map,body(createTransposePair))))))) `;`
      inferType `;`
      body(body(argument(argument(function(argumentOf(map,body(LCNF))))))) `;`
      inferType `;`
      body(body(argument(argument(function(argumentOf(map,body(argument(mapMapFBeforeTranspose)))))))) `;`
      inferType `;`
      body(body(argument(argument(RNF))))


    assert(oldTiling(input2D).get == simplified(input2D).get)
    assert(oldTiling(input2D).get == normalizedModified(input2D).get)
  }

  test("RNF did not normalize") {
    val expr2 = `apply`(join, `apply`(`apply`(map, lambda(identifier("η125"), `apply`(`apply`(map, lambda(identifier("ee3"), `apply`(join, `apply`(`apply`(map, lambda(identifier("η124"), `apply`(`apply`(map, lambda(identifier("η123"), `apply`(identifier("ee2"), identifier("η123")))), identifier("η124")))), `apply`(depApply[NatKind](split, 4), identifier("ee3")))))), identifier("η125")))), `apply`(depApply[NatKind](split, 4), identifier("ee1"))))
    val expr5 = `apply`(join, `apply`(`apply`(map, lambda(identifier("η141"), `apply`(`apply`(map, lambda(identifier("η140"), `apply`(join, identifier("η140")))), identifier("η141")))), `apply`(`apply`(map, lambda(identifier("η145"), `apply`(`apply`(map, lambda(identifier("η144"), `apply`(`apply`(map, lambda(identifier("η143"), `apply`(`apply`(map, lambda(identifier("η142"), `apply`(identifier("ee2"), identifier("η142")))), identifier("η143")))), identifier("η144")))), identifier("η145")))), `apply`(`apply`(map, lambda(identifier("η147"), `apply`(`apply`(map, lambda(identifier("η146"), `apply`(depApply[NatKind](split, 4), identifier("η146")))), identifier("η147")))), `apply`(depApply[NatKind](split, 4), identifier("ee1"))))))

    assert(RNF(expr2).get == expr5)
  }

  test("id traversals") {
    val expr = fun(f => fun(g => map(f) >> map(g)))
    val id = strategies.basic.id[Lift]()

    assert(
      List(
        topdown(id).apply(expr),
        bottomup(id).apply(expr),
        downup(id).apply(expr),
        downup2(id,id).apply(expr),
        oncetd(id).apply(expr),
        oncebu(id).apply(expr),
        alltd(id).apply(expr),
        sometd(id).apply(expr),
        //innermost(id)(expr),
        somebu(id).apply(expr)
      ).forall(x => betaEtaEquals(x.get, expr))
    )
  }

  test("simple fusion") {
    val expr = fun(f => fun(g => map(f) >> map(g)))
    val gold = fun(f => fun(g => map(f >> g)))

    assert(
      List(
        oncetd(mapFusion).apply(expr),
        oncebu(mapFusion).apply(expr),
        alltd(mapFusion).apply(expr),
        sometd(mapFusion).apply(expr),
        somebu(mapFusion).apply(expr),
        topdown(`try`(mapFusion)).apply(expr),
        bottomup(`try`(mapFusion)).apply(expr)
      ).forall(x => betaEtaEquals(x.get, gold))
    )

  }
}
