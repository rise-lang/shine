package elevate.rise.strategies

import elevate.core._
import lift.core._
import lift.core.DSL._
import elevate.rise.rules.algorithmic._
import elevate.core.strategies.traversal._
import elevate.core.strategies.basic._
import elevate.rise.Rise
import elevate.rise.strategies.algorithmic._
import elevate.rise.strategies.normalForm._
import lift.core.types._

object traversal {

  def traverseSingleSubexpression: Strategy[Rise] => Rise => Option[RewriteResult[Rise]] =
    s => {
      case App(_,_) => throw new Exception("this should not happen")
      case Identifier(_) => None
      case l @ Lambda(x, e) => Some(s(e).mapSuccess(Lambda(x, _)(l.t)))
      case dl @ DepLambda(x, e) => x match {
        case n: NatIdentifier => Some(s(e).mapSuccess(DepLambda[NatKind](n, _)(dl.t)))
        case dt: DataTypeIdentifier => Some(s(e).mapSuccess(DepLambda[DataKind](dt, _)(dl.t)))
      }
      case da @ DepApp(f, x) => x match {
        case n: Nat => Some(s(f).mapSuccess(DepApp[NatKind](_, n)(da.t)))
        case dt: DataType => Some(s(f).mapSuccess(DepApp[DataKind](_, dt)(da.t)))
      }
      case Literal(_) => None
      case ff: ForeignFunction => None
      case _:Primitive => None
    }

  implicit object LiftTraversable extends elevate.core.strategies.Traversable[Rise] {
    override def all: Strategy[Rise] => Strategy[Rise] = s => {
      case ap @ App(f, e) => s(f).flatMapSuccess(a => s(e).mapSuccess(b => App(a, b)(ap.t)))

      case x => traverseSingleSubexpression(s)(x) match {
        case Some(r) => r
        case None => Success(x)
      }
    }


    override def one: Strategy[Rise] => Strategy[Rise] = oneHandlingState(false)
    override def oneUsingState: Strategy[Rise] => Strategy[Rise] = oneHandlingState(true)

    def oneHandlingState: Boolean => Strategy[Rise] => Strategy[Rise] =
      carryOverState => s => {
        case a @ App(f, e) => s(f) match {
          case Success(x: Rise) => Success(App(x, e)(a.t))
          case Failure(state) => if (carryOverState)
            state(e).mapSuccess(App(f, _)(a.t)) else
            s(e).mapSuccess(App(f, _)(a.t))
        }
        case x => traverseSingleSubexpression(s)(x) match {
          case Some(r) => r
          case None => Failure(s)
        }
      }

    override def some: Strategy[Rise] => Strategy[Rise] = s => {
      case a @ App(f, e) => (s(f), s(e)) match {
        case (Failure(_), Failure(_)) => Failure(s)
        case (x, y) => Success(App(x.getProgramOrElse(f), y.getProgramOrElse(e))(a.t))
      }

      case x => traverseSingleSubexpression(s)(x) match {
        case Some(r) => r
        case None => Failure(s)
      }
    }
  }

  case class body(s: Strategy[Rise]) extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
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

  case class function(s: Strategy[Rise]) extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case ap @ App(f, x) => s(f).mapSuccess(App(_, x)(ap.t))
      case _ => Failure(s)
    }
    override def toString = s"function($s)"
  }

  // todo move to meta package
  case class inBody(s: MetaStrategy[Rise]) extends MetaStrategy[Rise] {
    def apply(e: Strategy[Rise]): RewriteResult[Strategy[Rise]] = e match {
      case body(x: Strategy[Rise]) => s(x).mapSuccess(body)
      case _ => Failure(s)
    }
  }

  case class argument(s: Strategy[Rise]) extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case ap @ App(f, x) => s(x).mapSuccess(App(f, _)(ap.t))
      case _ => Failure(s)
    }
    override def toString = s"argument($s)"
  }

  case class argumentOf(p: Primitive, s: Strategy[Rise]) extends Strategy[Rise] {
    def apply(e: Rise): RewriteResult[Rise] = e match {
      case ap @ App(f, x) if f == p => s(x).mapSuccess(App(f, _)(ap.t))
      case _ => Failure(s)
    }
    override def toString = s"argumentOf($p,$s)"
  }

  // applying a strategy to an expression applied to a lift `map`. Example:
  // ((map λe14. (transpose ((map (map e12)) e14))) e13) // input expr
  //  (map λe14. (transpose ((map (map e12)) e14)))      // result of `function`
  //       λe14. (transpose ((map (map e12)) e14))       // result of `argument`
  //             (transpose ((map (map e12)) e14))       // result of 'body' -> here we can apply s
  def fmap: Strategy[Rise] => Strategy[Rise] = s => function(argumentOf(map, body(s)))

  // fmap applied for expressions in rewrite normal form:
  // fuse -> fmap -> fission
  def fmapRNF: Strategy[Rise] => Strategy[Rise] =
    s => LCNF `;` mapFusion `;`
      LCNF `;` fmap(s) `;`
      LCNF `;` one(mapFullFission)

  // applying a strategy to an expression nested in one or multiple lift `map`s
  def mapped: Strategy[Rise] => Strategy[Rise] =
    s => s <+ (e => fmapRNF(mapped(s))(e))

  // moves along RNF-normalized expression
  // e.g., expr == ***f o ****g o *h
  // move(0)(s) == s(***f o ****g o *h)
  // move(1)(s) == s(****g o *h)
  // move(2)(s) == s(*h)
  def moveTowardsArgument: Int => Strategy[Rise] => Strategy[Rise] =
    i => s => applyNTimes(i)((e: Strategy[Rise]) => argument(e))(s)
}
