package elevate.lift.strategies

import elevate.core._
import lift.core._
import lift.core.DSL._
import elevate.lift.rules.algorithmic._
import elevate.core.strategies.traversal._
import elevate.core.strategies.basic._
import elevate.lift.strategies.algorithmic._
import elevate.lift.strategies.normalForm._
import lift.core.types._

object traversal {

  def traverseSingleSubexpression: Strategy[Lift] => Lift => Option[RewriteResult[Lift]] =
    s => {
      case Apply(_,_) => throw new Exception("this should not happen")
      case Identifier(_) => None
      case l @ Lambda(x, e) => Some(s(e).mapSuccess(Lambda(x, _)(l.t)))
      case dl @ DepLambda(x, e) => x match {
        case n: NatIdentifier => Some(s(e).mapSuccess(DepLambda[NatKind](n, _)(dl.t)))
        case dt: DataTypeIdentifier => Some(s(e).mapSuccess(DepLambda[DataKind](dt, _)(dl.t)))
      }
      case da @ DepApply(f, x) => x match {
        case n: Nat => Some(s(f).mapSuccess(DepApply[NatKind](_, n)(da.t)))
        case dt: DataType => Some(s(f).mapSuccess(DepApply[DataKind](_, dt)(da.t)))
      }
      case Literal(_) => None
      case ff: ForeignFunction => None
      case _:Primitive => None
    }

  implicit object LiftTraversable extends elevate.core.strategies.Traversable[Lift] {
    override def all: Strategy[Lift] => Strategy[Lift] = s => {
      case ap @ Apply(f, e) => s(f).flatMapSuccess(a => s(e).mapSuccess(b => Apply(a, b)(ap.t)))

      case x => traverseSingleSubexpression(s)(x) match {
        case Some(r) => r
        case None => Success(x)
      }
    }

    override def oneHandlingState: Boolean => Strategy[Lift] => Strategy[Lift] =
      carryOverState => s => {
        case a @ Apply(f, e) => s(f) match {
          case Success(x: Lift) => Success(Apply(x, e)(a.t))
          case Failure(state) => if (carryOverState)
            state(e).mapSuccess(Apply(f, _)(a.t)) else
            s(e).mapSuccess(Apply(f, _)(a.t))
        }
        case x => traverseSingleSubexpression(s)(x) match {
          case Some(r) => r
          case None => Failure(s)
        }
      }

    override def some: Strategy[Lift] => Strategy[Lift] = s => {
      case a @ Apply(f, e) => (s(f), s(e)) match {
        case (Failure(_), Failure(_)) => Failure(s)
        case (x, y) => Success(Apply(x.getProgramOrElse(f), y.getProgramOrElse(e))(a.t))
      }

      case x => traverseSingleSubexpression(s)(x) match {
        case Some(r) => r
        case None => Failure(s)
      }
    }
  }

  /*
  case class inTyped(s: Elevate) extends Elevate {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case TypedExpr(e, t) => s(e).mapSuccess(TypedExpr(_, t) )
      case _ => Failure(s)
    }
    override def toString = s"inTyped($s)"
  }
  */

  case class body(s: Elevate) extends Elevate {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case Lambda(x, f) => s(f).mapSuccess(Lambda(x, _)(e.t))
      case DepLambda(x: NatIdentifier, f) => s(f).mapSuccess(DepLambda[NatKind](x, _)(e.t))
      case DepLambda(x: DataTypeIdentifier, f) => s(f).mapSuccess(DepLambda[DataKind](x, _)(e.t))
      case DepLambda(x: AddressSpaceIdentifier, f) => s(f).mapSuccess(DepLambda[AddressSpaceKind](x, _)(e.t))
      case DepLambda(x: NatToNatIdentifier, f) => s(f).mapSuccess(DepLambda[NatToNatKind](x, _)(e.t))
      case DepLambda(x: NatToDataIdentifier, f) => s(f).mapSuccess(DepLambda[NatToDataKind](x, _)(e.t))
      case _ => Failure(s)
    }
    override def toString = s"body($s)"
  }

  case class function(s: Elevate) extends Elevate {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case Apply(f, e) => s(f).mapSuccess(Apply(_, e)(e.t))
      case _ => Failure(s)
    }
    override def toString = s"function($s)"
  }

  // todo move to meta package
  case class inBody(s: Meta) extends Strategy[Elevate] {
    def apply(e: Elevate): RewriteResult[Elevate] = e match {
      case body(x: Elevate) => s(x).mapSuccess(body)
      case _ => Failure(s)
    }
  }

  case class argument(s: Elevate) extends Elevate {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case Apply(f, e) => s(e).mapSuccess(Apply(f, _)(e.t))
      case _ => Failure(s)
    }
    override def toString = s"argument($s)"
  }

  case class argumentOf(x: Primitive, s: Elevate) extends Elevate {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case Apply(f, e) if f == x => s(e).mapSuccess(Apply(f, _)(e.t))
      case _ => Failure(s)
    }
    override def toString = s"argumentOf($x,$s)"
  }

  // applying a strategy to an expression applied to a lift `map`. Example:
  // ((map λe14. (transpose ((map (map e12)) e14))) e13) // input expr
  //  (map λe14. (transpose ((map (map e12)) e14)))      // result of `function`
  //       λe14. (transpose ((map (map e12)) e14))       // result of `argument`
  //             (transpose ((map (map e12)) e14))       // result of 'body' -> here we can apply s
  def fmap: Elevate => Elevate = s => function(argumentOf(map, body(s)))

  // fmap applied for expressions in rewrite normal form:
  // fuse -> fmap -> fission
  def fmapRNF: Elevate => Elevate =
    s => LCNF `;` mapFusion `;`
      LCNF `;` fmap(s) `;`
      LCNF `;` one(mapFullFission)

  // applying a strategy to an expression nested in one or multiple lift `map`s
  def mapped: Elevate => Elevate =
    s => s <+ (e => fmapRNF(mapped(s))(e))

  // moves along RNF-normalized expression
  // e.g., expr == ***f o ****g o *h
  // move(0)(s) == s(***f o ****g o *h)
  // move(1)(s) == s(****g o *h)
  // move(2)(s) == s(*h)
  def moveTowardsArgument: Int => Elevate => Elevate =
    i => s => applyNTimes(i)((e: Elevate) => argument(e))(s)
}
