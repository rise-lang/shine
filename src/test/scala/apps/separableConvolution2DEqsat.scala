package apps

import apps.separableConvolution2D._
import elevate.core._
import elevate.core.strategies.predicate._
import rise.core.DSL._
import rise.core._
import rise.elevate.Rise
import rise.elevate.rules._
import rise.elevate.rules.algorithmic._
import rise.elevate.strategies.algorithmic._
import rise.elevate.rules.traversal._
import rise.elevate.rules.movement._
import rise.elevate.util.makeClosed

import rise.eqsat.{rules => eqr}
import rise.eqsat.{strategies => eqs}

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}

class separableConvolution2DEqsat extends test_util.Tests {
  private val weights2d = binomialWeights2d
  private val weightsV = binomialWeightsV
  private val weightsH = binomialWeightsH

  private val separateDot: Strategy[Rise] =
    separateDotHV(weights2d, weightsH, weightsV)

  private val separateDotT: Strategy[Rise] =
    separateDotVH(weights2d, weightsV, weightsH)

  private val BENF = rise.elevate.strategies.normalForm.BENF()(alternative.RiseTraversable)

  // FIXME: hashCode() implementation is broken in Expr with regards to alpha-renaming
  case class ExprWrapper(e: Expr) {
    override def hashCode(): Int = customHash(e)
  }

  private def customHash: Expr => Int = {
    import semantics._
    {
      case _: Identifier => 17
      case Lambda(_, e) => 3 * customHash(e) + 1
      case App(f, e) => 5 * customHash(f) + -7 * customHash(e) + 2
      case DepLambda(_, e) => 4 * customHash(e) + 3
      case DepApp(f, _) => 6 * customHash(f) + 4
      case l @ Literal(_: ScalarData | _: VectorData) => l.d.hashCode()
      case Literal(_: NatData) => 91
      case Literal(_: IndexData) => 93
      case Literal(_: ArrayData) => 95
      case Literal(_: PairData) => 97
      case p: Primitive => p.getClass.hashCode()
    }
  }

  private def mayApply(s: Strategy[Rise], p: Rise): Option[Rise] = {
    s(p) match {
      case Success(p) => Some(p)
      case Failure(_) => None
    }
  }

  type ExpandStrategy = Rise => Seq[Rise]
  def everywhere(s: Strategy[Rise]): ExpandStrategy = { p =>
    import rise.core.types._
    mayApply(s, p).toSeq ++ (p match {
      case App(f, e) => everywhere(s)(f).map(App(_, e)(p.t)) ++ everywhere(s)(e).map(App(f, _)(p.t))
      case Identifier(_) => Nil
      case Lambda(x, e) => everywhere(s)(e).map(Lambda(x, _)(p.t))
      case DepLambda(x, e) => x match {
        case n: NatIdentifier => everywhere(s)(e).map(DepLambda[NatKind](n, _)(p.t))
        case n: DataTypeIdentifier => everywhere(s)(e).map(DepLambda[DataKind](n, _)(p.t))
        case n: AddressSpaceIdentifier => everywhere(s)(e).map(DepLambda[AddressSpaceKind](n, _)(p.t))
      }
      case DepApp(f, x) => everywhere(s)(f).map(DepApp(_, x)(p.t))
      case Literal(_) => Nil
      case _: ForeignFunction => Nil
      case _: Primitive => Nil
    })
  }

  def maxTriggers(n: Int): Strategy[Rise] => Strategy[Rise] = s => p =>
    elevate.core.strategies.traversal.skip(n)(s)(alternative.RiseTraversable)(p) match {
      case Success(_) => Failure(maxTriggers(n)(s))
      case Failure(_) => Success(p)
    }

  private def find_rewrite_path(start: Rise, goal: Rise,
                                expandStrats: immutable.Seq[ExpandStrategy],
                                filterStrat: Strategy[Rise] = BENF): Unit = {
    // FIXME: ad-hoc closing mechanism
    val closedStart = makeClosed(BENF(start).get !: start.t)._1
    val closedGoal = makeClosed(eraseType(BENF(goal).get) !: start.t)._1

    val visited = mutable.Set[ExprWrapper]()
    val unvisited = mutable.Set[ExprWrapper](ExprWrapper(closedStart))
    @tailrec
    def expand(fuel: Int): Unit = {
      val finished = fuel <= 0 || unvisited.isEmpty || unvisited.contains(ExprWrapper(closedGoal))
      visited ++= unvisited
      if (finished) {
        println(s"fuel left: $fuel")
      } else {
        val expansion = expandStrats.flatMap { s =>
          unvisited.iterator.flatMap(c => s(c.e)).flatMap(c => mayApply(filterStrat, c).map(ExprWrapper))
        }
        println(s"expanded by ${expansion.size}")
        unvisited.clear()
        unvisited ++= expansion
        println(s"with ${unvisited.size} uniques")
        unvisited --= visited
        println(s"with ${unvisited.size} unvisited")
        expand(fuel - 1)
      }
    }

    util.printTime("expansion time", expand(10))
    println(s"found ${visited.size} candidates")
    assert(visited.contains(ExprWrapper(closedGoal)),
      "could not find rewrite path")
  }

  private def findRewritePath2(start: Rise, goal: Rise,
                               strat: eqs.Strategy,
                               initS: Strategy[Rise] = BENF): Unit = {
    import rise.eqsat.ExprSet
    // FIXME: ad-hoc closing mechanism
    val closedStart = makeClosed(initS(start).get !: start.t)._1
    val closedGoal = makeClosed(eraseType(initS(goal).get) !: start.t)._1
    //util.dotPrintTmp("start", closedStart)
    //util.dotPrintTmp("goal", closedGoal)
    val set = ExprSet.init(closedStart)
    @tailrec
    def expand(fuel: Int): Unit = {
      val modified = util.printTime(s"strat $fuel", strat(set))
      println(s"${set.countRepresentations()} representations using ${set.countNodes()} nodes")
      util.dotPrintTmp("set", set)
      if (modified && fuel > 0 && !util.printTime("represents time", set.represents(closedGoal))) {
        expand(fuel - 1)
      } else {
        println(s"fuel left: $fuel")
      }
    }

    util.printTime("expansion time", expand(10))
    assert(set.represents(closedGoal), "could not find rewrite path")
  }

  //// algorithmic

  test("BENF additive eqsat") {
    val benf = util.printTime("elevate", BENF(base(weights2d)).get)
    findRewritePath2(base(weights2d), benf, eqs.normalize(eqs.any(
      eqs.applyTopDown(eqr.betaReduction),
      eqs.applyTopDown(eqr.etaReduction)
    )), strategies.basic.id)
  }

  test("BENF destructive") {
    val benf = util.printTime("elevate", BENF(base(weights2d)).get)
    findRewritePath2(base(weights2d), benf, eqs.normalize(eqs.any(
      eqs.applyTopDown(eqr.betaReduction, destructive = true),
      eqs.applyTopDown(eqr.etaReduction, destructive = true)
    )), strategies.basic.id)
  }

  test("base to factorise") {
    find_rewrite_path(base(weights2d), factorised(weightsV)(weightsH), immutable.Seq(
      everywhere(separateDot)
    ))
  }

  test("base to factorise 2") {
    findRewritePath2(base(weights2d), factorised(weightsV)(weightsH),
      eqs.applyTopDown(eqr.separateDotHV(weights2d, weightsH, weightsV))
    )
  }

  // FIXME: does not terminate given 5mn
  // expansion time: 6mn 38s 345ms
  // found 13200 candidates
  ignore("base to scanline") {
    find_rewrite_path(base(weights2d), scanline(weightsV)(weightsH), immutable.Seq(
      everywhere(separateDotT),
      everywhere(`*f >> S -> S >> **f`),
      everywhere(mapFusion),
      everywhere(`*S >> T -> T >> S >> *T`),
      everywhere(removeTransposePair),
      everywhere(mapFirstFission),
      everywhere(`S >> **f -> *f >> S`)
    ), BENF `;` // FIXME: reduces the search but these bounds are hard to know in advance
      maxTriggers(10)(isEqualTo(primitives.map.primitive)) `;`
      maxTriggers(3)(isEqualTo(primitives.transpose.primitive)))
  }
/*
  // FIXME: something is not working
  test("base to scanline 2") {
    findRewritePath2(base(weights2d), scanline(weightsV)(weightsH), immutable.Seq(
      rise.eqsat.rules.separateDotVH(weights2d, weightsH, weightsV),
      rise.eqsat.rules.slideBeforeMap,
      rise.eqsat.rules.mapFusion,
      rise.eqsat.rules.mapSlideBeforeTranspose,
      rise.eqsat.rules.removeTransposePair,
      rise.eqsat.rules.mapFission,
      rise.eqsat.rules.slideBeforeMapMapF,
      rise.eqsat.rules.destructiveBetaReduction,
      rise.eqsat.rules.destructiveEtaReduction
    ))
  }
*/
  test("scanline to separated") {
    find_rewrite_path(scanline(weightsV)(weightsH), separated(weightsV)(weightsH), immutable.Seq(
      everywhere(mapLastFission()(default.RiseTraversable)),
      everywhere(mapFusion)
    ))
  }

  // FIXME: something is not working
  ignore("scanline to separated 2") {
    findRewritePath2(scanline(weightsV)(weightsH), separated(weightsV)(weightsH), eqs.seqWithNorm(
      eqs.normalize(eqs.any(
        eqs.applyTopDown(eqr.betaReduction, destructive = true),
        eqs.applyTopDown(eqr.etaReduction, destructive = true)
      )),
      eqs.applyTopDown(eqr.mapFission),
      eqs.applyTopDown(eqr.mapFusion)
    ))
  }

  //// lowering

  // TODO: read/write annotations would enable to trigger less lowerings here
  test("base to baseSeq") {
    find_rewrite_path(base(weights2d), baseSeq(weights2d), immutable.Seq(
      everywhere(lowering.reduceSeqUnroll),
      everywhere(lowering.mapSeq)
    ), BENF `;` maxTriggers(2)(isEqualTo(primitives.mapSeq.primitive)))
  }

  test("base to baseSeq 2") {
    findRewritePath2(base(weights2d), baseSeq(weights2d), eqs.seq(
      eqs.applyTopDown(eqr.lowering.reduceSeqUnroll),
      eqs.applyTopDown(eqr.lowering.mapSeq)
    ))
  }

  test("factorised to factorisedSeq") {
    find_rewrite_path(factorised(weightsV)(weightsH), factorisedSeq(weightsV)(weightsH), immutable.Seq(
      everywhere(lowering.reduceSeqUnroll),
      everywhere(lowering.mapSeq)
    ),  BENF `;` maxTriggers(2)(isEqualTo(primitives.mapSeq.primitive)))
  }

  test("factorised to factorisedSeq 2") {
    findRewritePath2(factorised(weightsV)(weightsH), factorisedSeq(weightsV)(weightsH), eqs.seq(
      eqs.applyTopDown(eqr.lowering.reduceSeqUnroll),
      eqs.applyTopDown(eqr.lowering.mapSeq)
    ))
  }

  // expansion time: 5s 629ms
  // found 516 candidates
  test("separated to separatedSeq") {
    find_rewrite_path(separated(weightsV)(weightsH), separatedSeq(weightsV)(weightsH), immutable.Seq(
      everywhere(lowering.reduceSeqUnroll),
      everywhere(lowering.mapSeq),
      everywhere(lowering.toMemAfterMapSeq)
    ), BENF `;`
      maxTriggers(4)(isEqualTo(primitives.mapSeq.primitive)) `;`
      maxTriggers(1)(isEqualTo(primitives.toMem.primitive)))
  }

  // expansion time: 47ms
  test("separated to separatedSeq 2") {
    findRewritePath2(separated(weightsV)(weightsH), separatedSeq(weightsV)(weightsH), eqs.seq(
      eqs.applyTopDown(eqr.lowering.reduceSeqUnroll),
      eqs.applyTopDown(eqr.lowering.mapSeq),
      eqs.applyTopDown(eqr.lowering.toMemAfterMapSeq)
    ))
  }

  test("scanline to scanlineSeq") {
    find_rewrite_path(scanline(weightsV)(weightsH), scanlineSeq(weightsV)(weightsH), immutable.Seq(
      everywhere(lowering.reduceSeqUnroll),
      everywhere(lowering.mapSeq)
    ), BENF `;` maxTriggers(3)(isEqualTo(primitives.mapSeq.primitive)))
  }

  test("scanline to scanlineSeq 2") {
    findRewritePath2(scanline(weightsV)(weightsH), scanlineSeq(weightsV)(weightsH), eqs.seq(
      eqs.applyTopDown(eqr.lowering.reduceSeqUnroll),
      eqs.applyTopDown(eqr.lowering.mapSeq),
    ))
  }

  test("scanline to regRotSeq") {
    find_rewrite_path(scanline(weightsV)(weightsH), regRotSeq(weightsV)(weightsH), immutable.Seq(
      everywhere(lowering.reduceSeqUnroll),
      everywhere(lowering.mapSeq),
      everywhere(lowering.rotateValues(fun(x => x))),
      everywhere(lowering.iterateStream)
    ), BENF `;` // this filtering reduces candidates from 4260 to 516
      maxTriggers(1)(isEqualTo(primitives.mapSeq.primitive)) `;`
      maxTriggers(1)(isEqualTo(primitives.rotateValues.primitive)) `;`
      maxTriggers(1)(isEqualTo(primitives.iterateStream.primitive)))
  }

  test("scanline to regRotSeq 2") {
    findRewritePath2(scanline(weightsV)(weightsH), regRotSeq(weightsV)(weightsH), eqs.seq(
      eqs.applyTopDown(eqr.lowering.reduceSeqUnroll),
      eqs.applyTopDown(eqr.lowering.mapSeq),
      eqs.applyTopDown(eqr.lowering.rotateValues(fun(x => x))),
      eqs.applyTopDown(eqr.lowering.iterateStream)
    ))
  }
}