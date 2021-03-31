package rise.elevate

import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import rise.elevate.util._
import rise.core.DSL._
import rise.core.primitives._
import rise.core.types.NatKind
import rise.elevate.meta.fission.bodyFission
import rise.elevate.meta.traversal.inBody
import rise.elevate.rules.algorithmic._
import rise.elevate.rules.movement._
import rise.elevate.rules.traversal.default._
import rise.elevate.rules.traversal.{argument, argumentOf, body, function}


class traversals extends test_util.Tests {

  def tileND = rise.elevate.strategies.tiling.tileND(RiseTraversable)
  val DFNF = rise.elevate.strategies.normalForm.DFNF()(RiseTraversable)
  val RNF = rise.elevate.strategies.normalForm.RNF()(RiseTraversable)
  val FNF = rise.elevate.meta.fission.FNF(rise.elevate.meta.traversal.MetaRiseTraversable(RiseTraversable))

  test("rewrite simple elevate strategy") {
    val expr = fun(f => fun(g => map(f) >> map(g)))
    val strategy = body(body(body(mapFusion `;` function(mapLastFission()))))

    val metaStrategy = inBody(inBody(bodyFission))(strategy)
    val newStrategy = metaStrategy.get
    assert(strategy != newStrategy)
    assert(makeClosed(newStrategy(expr).get) == makeClosed(strategy(expr).get))
  }

  test("simplification") {
    val input2D = λ(i => λ(f => **!(f) $ i))
    val orig = body(body(tileND(2)(tileSize)))
    logger.debug(orig.toString)

    val oldTiling = body(body(
      function(argumentOf(map.primitive, body(function(splitJoin(4)) `;` DFNF `;` RNF))) `;`
        function(splitJoin(4)) `;`
        DFNF `;` RNF `;` DFNF `;` RNF `;` DFNF `;`
        argument(argument(function(argumentOf(map.primitive, body(idAfter `;` createTransposePair `;` DFNF `;` argument(mapMapFBeforeTranspose())))) `;` DFNF `;` RNF)) `;`
        DFNF `;` RNF `;` DFNF `;` RNF `;` RNF
    ))

    val simplified = body(body(
      function(argumentOf(map.primitive, body(function(splitJoin(4))))) `;`
        function(splitJoin(4)) `;`
        RNF `;` DFNF `;`
        argument(argument(function(argumentOf(map.primitive, body(idAfter `;` createTransposePair `;` DFNF `;` argument(mapMapFBeforeTranspose())))) `;` RNF))))

    val normalized = FNF(simplified).get
    logger.debug(normalized)
    val normalizedModified = body(body(function(argumentOf(map.primitive, body(function(splitJoin(4))))))) `;`
      body(body(function(splitJoin(4)))) `;`
      body(body(RNF)) `;`
      body(body(DFNF)) `;`
      body(body(argument(argument(function(argumentOf(map.primitive, body(idAfter))))))) `;`
      body(body(argument(argument(function(argumentOf(map.primitive, body(createTransposePair))))))) `;`
      body(body(argument(argument(function(argumentOf(map.primitive, body(DFNF))))))) `;`
      body(body(argument(argument(function(argumentOf(map.primitive, body(argument(mapMapFBeforeTranspose())))))))) `;`
      body(body(argument(argument(RNF))))


    assert(makeClosed(oldTiling(input2D).get) == makeClosed(simplified(input2D).get))
    assert(makeClosed(oldTiling(input2D).get) == makeClosed(normalizedModified(input2D).get))
  }

  test("RNF did not normalize") {
    val expr2 = lambda(identifier("ee1"), lambda(identifier("ee2"), app(join, app(app(map, lambda(identifier("η125"), app(app(map, lambda(identifier("ee3"), app(join, app(app(map, lambda(identifier("η124"), app(app(map, lambda(identifier("η123"), app(identifier("ee2"), identifier("η123")))), identifier("η124")))), app(depApp[NatKind](split, 4), identifier("ee3")))))), identifier("η125")))), app(depApp[NatKind](split, 4), identifier("ee1"))))))
    val expr5 = lambda(identifier("ee1"), lambda(identifier("ee2"), app(join, app(app(map, lambda(identifier("η141"), app(app(map, lambda(identifier("η140"), app(join, identifier("η140")))), identifier("η141")))), app(app(map, lambda(identifier("η145"), app(app(map, lambda(identifier("η144"), app(app(map, lambda(identifier("η143"), app(app(map, lambda(identifier("η142"), app(identifier("ee2"), identifier("η142")))), identifier("η143")))), identifier("η144")))), identifier("η145")))), app(app(map, lambda(identifier("η147"), app(app(map, lambda(identifier("η146"), app(depApp[NatKind](split, 4), identifier("η146")))), identifier("η147")))), app(depApp[NatKind](split, 4), identifier("ee1"))))))))

    assert(makeClosed(RNF(expr2).get) == makeClosed(toExpr(expr5)))
  }

  test("id traversals") {
    val expr = fun(f => fun(g => map(f) >> map(g)))
    val id = elevate.core.strategies.basic.id[Rise]

    assert(
      List(
        allTopdown(id).apply(expr),
        allBottomup(id).apply(expr),
        downup(id).apply(expr),
        downup2(id,id).apply(expr),
        topDown(id).apply(expr),
        bottomUp(id).apply(expr),
        alltd(id).apply(expr),
        sometd(id).apply(expr),
        somebu(id).apply(expr)
      ).forall(x => betaEtaEquals(x.get, expr))
    )
  }

  test("simple fusion") {
    val expr = fun(f => fun(g => map(f) >> map(g)))
    val gold = fun(f => fun(g => map(f >> g)))

    assert(
      List(
        topDown(mapFusion).apply(expr),
        bottomUp(mapFusion).apply(expr),
        alltd(mapFusion).apply(expr),
        sometd(mapFusion).apply(expr),
        somebu(mapFusion).apply(expr),
        allTopdown(`try`(mapFusion)).apply(expr),
        allBottomup(`try`(mapFusion)).apply(expr)
      ).forall(x => betaEtaEquals(x.get, gold))
    )

  }
}
