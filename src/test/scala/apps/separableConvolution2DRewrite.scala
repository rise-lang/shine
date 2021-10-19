package apps

import separableConvolution2D._
import rise.core._
import rise.core.DSL._
import HighLevelConstructs._
import elevate.core._
import rise.elevate.rules._
import rise.elevate.rules.algorithmic._
import rise.elevate.rules.movement._
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.macros.StrategyMacro.strategy
import rise.elevate.Rise
import rise.elevate.strategies.algorithmic._
import rise.elevate.rules.traversal._
import rise.elevate.rules.traversal.alternative._
import rise.core.primitives._

class separableConvolution2DRewrite extends test_util.Tests {
  private val idE: Expr = fun(x => x)
  private val idS: Strategy[Rise] = strategies.basic.id
  private val idSC: Strategy[(Rise, Int)] = strategies.basic.id

  private val weights2d = binomialWeights2d
  private val weightsV = binomialWeightsV
  private val weightsH = binomialWeightsH

  private val * : ToBeTyped[Rise] = map
  private val T: ToBeTyped[Rise] = transpose
  private val P = padClamp2D(1)
  private val Sh = slide(3)(1)
  private val Sv = slide(3)(1)
  private val Dh = dot(weightsH)
  private val Dv = dot(weightsV)

  private val BENF = rise.elevate.strategies.normalForm.BENF()(alternative.RiseTraversable)


  def countApplications(s: Strategy[Rise]): Strategy[(Rise, Int)] = { case (p, c) =>
    s(p) match {
      case Success(p2) => Success((p2, c + 1))
      case Failure(s) => Failure(countApplications(s))
    }
  }

  // FIXME: lots of code duplication
  implicit val traversableCountApplications: strategies.Traversable[(Rise, Int)] = new strategies.Traversable[(Rise, Int)] {
    override def all: Strategy[(Rise, Int)] => Strategy[(Rise, Int)] = s => {
      case (ap @ App(f, e), c) =>
        s((f, c)).flatMapSuccess { case (f2, c2) => s((e, c2)).mapSuccess { case (e2, c3) =>
          (App(f2, e2)(ap.t), c3) }}
      case x => traverseSingleSubexpression(s)(x) match {
        case Some(r) => r
        case None => Failure(s)
      }
    }

    override def one: Strategy[(Rise, Int)] => Strategy[(Rise, Int)] =
      oneHandlingState(false)
    override def oneUsingState: Strategy[(Rise, Int)] => Strategy[(Rise, Int)] =
      oneHandlingState(true)

    private def oneHandlingState: Boolean => Strategy[(Rise, Int)] => Strategy[(Rise, Int)] =
      carryOverState => s => {
        // (option 2) traverse to function first
        case (a @ App(f, e), c) => s((f, c)) match {
          case Success((x: Rise, c2)) => Success((App(x, e)(a.t), c2))
          case Failure(state) => if (carryOverState)
            mapSuccessWithCount(state((e, c)), App(f, _)(a.t)) else
            mapSuccessWithCount(s((e, c)), App(f, _)(a.t))
        }

        // Push s further down the AST.
        // If there are no subexpressions (None),
        // we failed to apply s once => Failure
        case x => traverseSingleSubexpression(s)(x) match {
          case Some(r) => r
          case None => Failure(s)
        }
      }

    override def some: Strategy[(Rise, Int)] => Strategy[(Rise, Int)] = s => {
      case (a @ App(f, e), c) => (s((f, 0)), s((e, 0))) match {
        case (Failure(_), Failure(_)) => Failure(s)
        case (x, y) =>
          val (f2, cf) = x.getProgramOrElse((f, 0))
          val (e2, ce) = y.getProgramOrElse((e, 0))
          Success((App(f2, e2)(a.t), c + cf + ce))
      }

      // ...same here (see oneHandlingState)
      case x => traverseSingleSubexpression(s)(x) match {
        case Some(r) => r
        case None => Failure(s)
      }
    }

    // Handle all Rise-AST nodes that contain one
    // or no subexpressions (all except App)
    type TraversalType = Strategy[(Rise, Int)] => ((Rise, Int)) => Option[RewriteResult[(Rise, Int)]]
    private def mapSuccessWithCount(r: RewriteResult[(Rise, Int)], f: Rise => Rise): RewriteResult[(Rise, Int)] =
      r.mapSuccess { case (e2, c2) => (f(e2), c2) }
    protected def traverseSingleSubexpression: TraversalType = {
      import rise.core.types._
      s => {
        case (App(_,_), _) => throw new Exception("this should not happen")
        case (Identifier(_), _) => None
        case (l @ Lambda(x, e), c) => Some(mapSuccessWithCount(s((e, c)), Lambda(x, _)(l.t)))
        case (dl @ DepLambda(x, e), c) => x match {
          case n: NatIdentifier =>
            Some(mapSuccessWithCount(s((e, c)), DepLambda[NatKind](n, _)(dl.t)))
          case dt: DataTypeIdentifier =>
            Some(mapSuccessWithCount(s((e, c)), DepLambda[DataKind](dt, _)(dl.t)))
          case addr: AddressSpaceIdentifier =>
            Some(mapSuccessWithCount(s((e, c)), DepLambda[AddressSpaceKind](addr, _)(dl.t)))
        }
        case (da @ DepApp(f, x), c) => x match {
          case n: Nat => Some(mapSuccessWithCount(s((f, c)), DepApp[NatKind](_, n)(da.t)))
          case dt: DataType =>
            Some(mapSuccessWithCount(s((f, c)), DepApp[DataKind](_, dt)(da.t)))
          case addr: AddressSpace =>
            Some(mapSuccessWithCount(s((f, c)), DepApp[AddressSpaceKind](_, addr)(da.t)))
          case n2n: NatToNat =>
            Some(mapSuccessWithCount(s((f, c)), DepApp[NatToNatKind](_, n2n)(da.t)))
          case n2d: NatToData =>
            Some(mapSuccessWithCount(s((f, c)), DepApp[NatToDataKind](_, n2d)(da.t)))
        }
        case (Literal(_), _) => None
        case (_: TypeAnnotation, _) => throw new Exception("Type annotations should be gone.")
        case (_: TypeAssertion, _) => throw new Exception("Type assertions should be gone.")
        case (_: Opaque, _) => throw new Exception("Opaque expressions should be gone.")
        case (_: Primitive, _) => None
      }
    }
  }

  @strategy def BENF_counting: Strategy[(Rise, Int)] =
    normalize(traversableCountApplications)(countApplications(etaReduction()) <+ countApplications(betaReduction))

  private def ben_eq(a: Expr, b: Expr): Boolean = {
    val na = BENF(a).get
    val nb = BENF(b).get
    val uab: Rise = toBeTyped(na) !: nb.t
    makeClosed(uab) =~~= makeClosed(nb)
  }

  private val separateDot: Strategy[Rise] =
    separateDotHV(weights2d, weightsH, weightsV)

  private val separateDotT: Strategy[Rise] =
    separateDotVH(weights2d, weightsV, weightsH)

  private def assert_ben_eq(a: Expr, b: Expr): Unit =
    if (!ben_eq(a, b)) {
      throw new Exception(s"expected structural equality:\n" +
        s"Got:\n${BENF(a).get}\nExpected:\n${BENF(b).get}")
    }

  private def rewrite_steps(a: Expr, steps: scala.collection.Seq[(Strategy[Rise], Expr)]): Unit = {
    steps.foldLeft[Expr](a)({ case (e, (s, expected)) =>
      val debug = BENF(e).get
      val result = s(debug).get
      assert_ben_eq(result, expected)
      result
    })
  }

  private def rewrite_steps_counting(a: Expr, steps: scala.collection.Seq[(Strategy[(Rise, Int)], Expr)]): Unit = {
    var totalBENFC = 0
    val (_, endC) = steps.foldLeft[(Expr, Int)]((a, 0))({ case ((p, c), (s, expected)) =>
      val (norm, benfC) = BENF_counting((p, 0)).get
      totalBENFC += benfC
      val result = s((norm, c)).get
      assert_ben_eq(result._1, expected)
      result
    })
    println(s"$endC rewrite rules applied, as well as $totalBENFC for BENF normalization")
  }

  //// algorithmic

  test("base to factorise") {
    rewrite_steps(base(weights2d), scala.collection.Seq(
      topDown(separateDot) -> factorised(weightsV)(weightsH)
    ))
  }

  test("base to scanline") {
    rewrite_steps(base(weights2d), scala.collection.Seq(
      idS
        -> (P >> *(Sh) >> Sv >> *(T) >> *(*(fun(nbh => dot(join(weights2d))(join(nbh)))))),
      topDown(separateDotT)
        -> (P >> *(Sh) >> Sv >> *(T) >> *(*(T >> *(Dv) >> Dh))),
      topDown(`*f >> S -> S >> **f`)
        -> (P >> Sv >> *(*(Sh)) >> *(T) >> *(*(T >> *(Dv) >> Dh))),
      topDown(mapFusion)
        -> (P >> Sv >> *(*(Sh)) >> *(T >> *(T >> *(Dv) >> Dh))),
      topDown(mapFusion)
        -> (P >> Sv >> *(*(Sh) >> T >> *(T >> *(Dv) >> Dh))),
      topDown(`*S >> T -> T >> S >> *T`)
        -> (P >> Sv >> *(T >> Sh >> *(T) >> *(T >> *(Dv) >> Dh))),
      topDown(mapFusion)
        -> (P >> Sv >> *(T >> Sh >> *(T >> T >> *(Dv) >> Dh))),
      topDown(removeTransposePair)
        -> (P >> Sv >> *(T >> Sh >> *(*(Dv) >> Dh))),
      skip(1)(mapFirstFission)
        -> (P >> Sv >> *(T >> Sh >> *(*(Dv)) >> *(Dh))),
      topDown(`S >> **f -> *f >> S`)
        -> (P >> Sv >> *(T >> *(Dv) >> Sh >> *(Dh))),
      idS
        -> scanline(weightsV)(weightsH)
    ))
  }

  test("scanline to separated") {
    rewrite_steps(scanline(weightsV)(weightsH), scala.collection.Seq(
      idS
        -> (P >> Sv >> *(T >> *(Dv) >> Sh >> *(Dh))),
      repeatNTimes(2)(topDown(mapFirstFission))
        -> (P >> Sv >> *(T) >> *(*(Dv)) >> *(Sh >> *(Dh))),
      skip(1)(mapFusion)
        -> (P >> Sv >> *(T >> *(Dv)) >> *(Sh >> *(Dh))),
      idS
        -> separated(weightsV)(weightsH)
    ))
  }

  test("base to scanline (mapLastFission)") {
    rewrite_steps_counting(base(weights2d), scala.collection.Seq(
      idSC
        -> (P >> *(Sh) >> Sv >> *(T) >> *(*(fun(nbh => dot(join(weights2d))(join(nbh)))))),
      topDown(countApplications(separateDotT))
        -> (P >> *(Sh) >> Sv >> *(T) >> *(*(T >> *(Dv) >> Dh))),
      topDown(countApplications(`*f >> S -> S >> **f`))
        -> (P >> Sv >> *(*(Sh)) >> *(T) >> *(*(T >> *(Dv) >> Dh))),
      topDown(countApplications(mapFusion))
        -> (P >> Sv >> *(*(Sh)) >> *(T >> *(T >> *(Dv) >> Dh))),
      topDown(countApplications(mapFusion))
        -> (P >> Sv >> *(*(Sh) >> T >> *(T >> *(Dv) >> Dh))),
      topDown(countApplications(`*S >> T -> T >> S >> *T`))
        -> (P >> Sv >> *(T >> Sh >> *(T) >> *(T >> *(Dv) >> Dh))),
      topDown(countApplications(mapFusion))
        -> (P >> Sv >> *(T >> Sh >> *(T >> T >> *(Dv) >> Dh))),
      topDown(countApplications(removeTransposePair))
        -> (P >> Sv >> *(T >> Sh >> *(*(Dv) >> Dh))),
      repeatNTimes(3)(skip(1)(countApplications(mapLastFission())))
        -> (P >> Sv >> *(T >> Sh >> *(*(Dv)) >> *(zip(weightsH)) >> *(*(mulT)) >> *(reduce(add)(lf32(0.0f))))),
      repeatNTimes(2)(topDown(countApplications(mapFusion)))
        -> (P >> Sv >> *(T >> Sh >> *(*(Dv)) >> *(Dh))),
      topDown(countApplications(`S >> **f -> *f >> S`))
        -> (P >> Sv >> *(T >> *(Dv) >> Sh >> *(Dh))),
      idSC
        -> scanline(weightsV)(weightsH)
    ))
  }

  test("scanline to separated (mapLastFission)") {
    rewrite_steps(scanline(weightsV)(weightsH), scala.collection.Seq(
      idS
        -> (P >> Sv >> *(T >> *(Dv) >> Sh >> *(Dh))),
      skip(0)(mapLastFission())
        -> (P >> Sv >> *(T >> *(Dv) >> Sh) >> *(*(Dh))),
      skip(1)(mapLastFission())
        -> (P >> Sv >> *(T >> *(Dv)) >> *(Sh) >> *(*(Dh))),
      skip(1)(mapLastFission())
        -> (P >> Sv >> *(T) >> *(*(Dv)) >> *(Sh) >> *(*(Dh))),
      skip(0)(mapFusion)
        -> (P >> Sv >> *(T) >> *(*(Dv)) >> *(Sh >> *(Dh))),
      skip(1)(mapFusion)
        -> (P >> Sv >> *(T >> *(Dv)) >> *(Sh >> *(Dh))),
      idS
        -> separated(weightsV)(weightsH)
    ))
  }

  //// lowering

  test("base to baseSeq") {
    rewrite_steps(base(weights2d), scala.collection.Seq(
      (topDown(lowering.reduceSeqUnroll) `;`
        repeatNTimes(2)(topDown(lowering.mapSeq)))
        -> baseSeq(weights2d)
    ))
  }

  test("factorised to factorisedSeq") {
    rewrite_steps(factorised(weightsV)(weightsH), scala.collection.Seq(
      (repeatNTimes(2)(topDown(lowering.reduceSeqUnroll)) `;`
        repeatNTimes(2)(topDown(lowering.mapSeq)))
        -> factorisedSeq(weightsV)(weightsH)
    ))
  }

  test("separated to separatedSeq") {
    rewrite_steps(separated(weightsV)(weightsH), scala.collection.Seq(
      (repeatNTimes(2)(topDown(lowering.reduceSeqUnroll)) `;`
        repeatNTimes(2)(topDown(lowering.mapSeq)) `;`
        repeatNTimes(2)(skip(1)(lowering.mapSeq)) `;`
        body(argument(lowering.toMemAfterMapSeq)))
        -> separatedSeq(weightsV)(weightsH)
    ))
  }

  test("scanline to scanlineSeq") {
    rewrite_steps(scanline(weightsV)(weightsH), scala.collection.Seq(
      (repeatNTimes(2)(topDown(lowering.reduceSeqUnroll)) `;`
        repeatNTimes(2)(topDown(lowering.mapSeq)) `;`
        skip(1)(lowering.mapSeq))
        -> scanlineSeq(weightsV)(weightsH)
    ))
  }

  test("scanline to regRotSeq") {
    rewrite_steps(scanline(weightsV)(weightsH), scala.collection.Seq(
      (repeatNTimes(2)(topDown(lowering.reduceSeqUnroll)) `;`
        topDown(lowering.mapSeq) `;`
        topDown(lowering.rotateValues(idE)) `;`
        topDown(lowering.iterateStream))
        -> regRotSeq(weightsV)(weightsH)
    ))
  }
}
