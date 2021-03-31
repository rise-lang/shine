package apps

import apps.separableConvolution2D._
import elevate.core._
import elevate.core.strategies.predicate._
import rise.core.DSL._
import rise.core._
import rise.core.equality._
import rise.elevate.Rise
import rise.elevate.rules._
import rise.elevate.rules.algorithmic._
import rise.elevate.strategies.algorithmic._
import rise.elevate.rules.traversal._
import rise.elevate.rules.movement._

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

  case class ExprWrapper(e: Expr) {
    override def hashCode(): Int = exprAlphaEq(typeErasure).hash(e)
    override def equals(o : Any) : Boolean = o match {
      case other : ExprWrapper => exprAlphaEq(typeAlphaEq).apply(this.e)(other.e)
      case other : Expr => exprAlphaEq(typeAlphaEq).apply(this.e)(other)
      case _ => false
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
        case n: NatIdentifier =>
          everywhere(s)(e).map(DepLambda[NatKind](n, _)(p.t))
        case n: DataTypeIdentifier =>
          everywhere(s)(e).map(DepLambda[DataKind](n, _)(p.t))
        case n: AddressSpaceIdentifier =>
          everywhere(s)(e).map(DepLambda[AddressSpaceKind](n, _)(p.t))
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
    val closedStart = makeClosed(BENF(start).get !: start.t)
    val closedGoal = makeClosed(eraseType(BENF(goal).get) !: start.t)

    val visited = mutable.Set[ExprWrapper]()
    val unvisited = mutable.Set[ExprWrapper](ExprWrapper(closedStart))
    @tailrec
    def expand(fuel: Int): Unit = {
      val finished = fuel <= 0 || unvisited.isEmpty || unvisited.contains(ExprWrapper(closedGoal))
      visited ++= unvisited
      if (finished) {
        logger.debug(s"fuel left: $fuel")
      } else {
        val expansion = expandStrats.flatMap { s =>
          unvisited.iterator.flatMap(c => s(c.e))
            .flatMap(c => mayApply(filterStrat, c).map(ExprWrapper))
        }
        logger.debug(s"expanded by ${expansion.size}")
        unvisited.clear()
        unvisited ++= expansion
        logger.debug(s"with ${unvisited.size} uniques")
        unvisited --= visited
        logger.debug(s"with ${unvisited.size} unvisited")
        expand(fuel - 1)
      }
    }

    util.printTime("expansion time", expand(10))
    logger.debug(s"found ${visited.size} candidates")
    assert(visited.contains(ExprWrapper(closedGoal)),
      "could not find rewrite path")
  }

  // -- algorithmic

  test("base to factorise") {
    find_rewrite_path(base(weights2d), factorised(weightsV)(weightsH), immutable.Seq(
      everywhere(separateDot)
    ))
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

  test("scanline to separated") {
    find_rewrite_path(scanline(weightsV)(weightsH), separated(weightsV)(weightsH), immutable.Seq(
      everywhere(mapLastFission()(default.RiseTraversable)),
      everywhere(mapFusion)
    ))
  }

  // -- lowering

  // TODO: read/write annotations would enable to trigger less lowerings here
  test("base to baseSeq") {
    find_rewrite_path(base(weights2d), baseSeq(weights2d), immutable.Seq(
      everywhere(lowering.reduceSeqUnroll),
      everywhere(lowering.mapSeq)
    ), BENF `;` maxTriggers(2)(isEqualTo(primitives.mapSeq.primitive)))
  }

  test("factorised to factorisedSeq") {
    find_rewrite_path(factorised(weightsV)(weightsH), factorisedSeq(weightsV)(weightsH),
      immutable.Seq(
        everywhere(lowering.reduceSeqUnroll),
        everywhere(lowering.mapSeq)
      ),
      BENF `;` maxTriggers(2)(isEqualTo(primitives.mapSeq.primitive)))
  }

  test("separated to separatedSeq") {
    find_rewrite_path(separated(weightsV)(weightsH), separatedSeq(weightsV)(weightsH),
      immutable.Seq(
        everywhere(lowering.reduceSeqUnroll),
        everywhere(lowering.mapSeq),
        everywhere(lowering.toMemAfterMapSeq)
      ), BENF `;`
        maxTriggers(4)(isEqualTo(primitives.mapSeq.primitive)) `;`
        maxTriggers(1)(isEqualTo(primitives.toMem.primitive)))
  }

  test("scanline to scanlineSeq") {
    find_rewrite_path(scanline(weightsV)(weightsH), scanlineSeq(weightsV)(weightsH), immutable.Seq(
      everywhere(lowering.reduceSeqUnroll),
      everywhere(lowering.mapSeq)
    ), BENF `;` maxTriggers(3)(isEqualTo(primitives.mapSeq.primitive)))
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
}

