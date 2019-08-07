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
import elevate.lift._
import elevate.lift.rules
import elevate.lift.rules.algorithmic._
import elevate.lift.strategies.predicate.isLambda
import elevate.lift.strategies.traversal.{body, function, inBody}
import elevate.meta.rules.fission.bodyFission
import lift.core.DSL._
import lift.core._
import lift.core.primitives.{join, map, split}
import lift.core.types.NatKind


class traversals extends idealised.util.Tests {

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


    //assert(betaEtaEquals(orig(input2D).get, printed(input2D).get))
    //println(printed(input2D).get)
  }

  test("RNF did not normalize") {
    val expr2 = Apply(join, Apply(Apply(map, Lambda(Identifier("η125"), Apply(Apply(map, Lambda(Identifier("ee3"), Apply(join, Apply(Apply(map, Lambda(Identifier("η124"), Apply(Apply(map, Lambda(Identifier("η123"), Apply(Identifier("ee2"), Identifier("η123")))), Identifier("η124")))), Apply(DepApply[NatKind](split, 4), Identifier("ee3")))))), Identifier("η125")))), Apply(DepApply[NatKind](split, 4), Identifier("ee1"))))
    val expr5 = Apply(join, Apply(Apply(map, Lambda(Identifier("η141"), Apply(Apply(map, Lambda(Identifier("η140"), Apply(join, Identifier("η140")))), Identifier("η141")))), Apply(Apply(map, Lambda(Identifier("η145"), Apply(Apply(map, Lambda(Identifier("η144"), Apply(Apply(map, Lambda(Identifier("η143"), Apply(Apply(map, Lambda(Identifier("η142"), Apply(Identifier("ee2"), Identifier("η142")))), Identifier("η143")))), Identifier("η144")))), Identifier("η145")))), Apply(Apply(map, Lambda(Identifier("η147"), Apply(Apply(map, Lambda(Identifier("η146"), Apply(DepApply[NatKind](split, 4), Identifier("η146")))), Identifier("η147")))), Apply(DepApply[NatKind](split, 4), Identifier("ee1"))))))

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
