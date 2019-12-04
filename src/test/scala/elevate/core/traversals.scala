package elevate.core

import elevate.rise.rules._
import elevate.util._
import elevate.rise.strategies.normalForm._
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.rise.strategies.traversal._
import elevate.meta.rules.fission._
import elevate.rise.rules.movement._
import elevate.rise.strategies.tiling._
import elevate.rise.strategies.util._
import elevate.rise._
import elevate.rise.rules
import elevate.rise.rules.algorithmic._
import elevate.rise.strategies.predicate.isLambda
import elevate.rise.strategies.traversal.{body, function, inBody}
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
    println(orig.toString)

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
    println(normalized.toString)
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
    val expr2 = app(join, app(app(map, lambda(identifier("η125"), app(app(map, lambda(identifier("ee3"), app(join, app(app(map, lambda(identifier("η124"), app(app(map, lambda(identifier("η123"), app(identifier("ee2"), identifier("η123")))), identifier("η124")))), app(depApp[NatKind](split, 4), identifier("ee3")))))), identifier("η125")))), app(depApp[NatKind](split, 4), identifier("ee1"))))
    val expr5 = app(join, app(app(map, lambda(identifier("η141"), app(app(map, lambda(identifier("η140"), app(join, identifier("η140")))), identifier("η141")))), app(app(map, lambda(identifier("η145"), app(app(map, lambda(identifier("η144"), app(app(map, lambda(identifier("η143"), app(app(map, lambda(identifier("η142"), app(identifier("ee2"), identifier("η142")))), identifier("η143")))), identifier("η144")))), identifier("η145")))), app(app(map, lambda(identifier("η147"), app(app(map, lambda(identifier("η146"), app(depApp[NatKind](split, 4), identifier("η146")))), identifier("η147")))), app(depApp[NatKind](split, 4), identifier("ee1"))))))

    assert(RNF(expr2).get == expr5)
  }

  test("id traversals") {
    val expr = fun(f => fun(g => map(f) >> map(g)))
    val id = strategies.basic.id[Rise]()

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
