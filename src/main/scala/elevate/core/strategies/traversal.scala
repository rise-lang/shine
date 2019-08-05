package elevate.core.strategies

import elevate.core._
import elevate.core.strategies.basic._
import lift.core.types._
import lift.core._

object traversal {

  // generic one-level traversal operators

  private def traverseSingleSubexpression: Strategy[Lift] => Lift => Option[RewriteResult[Lift]] =
    s => {
      case Identifier(_) => None
      case Lambda(x, e) => Some(s(e).mapSuccess(Lambda(x, _)))
      case DepLambda(x, e) => x match {
        case n: NatIdentifier => Some(s(e).mapSuccess(DepLambda[NatKind](n, _)))
        case dt: DataTypeIdentifier => Some(s(e).mapSuccess(DepLambda[DataKind](dt, _)))
      }
      case DepApply(f, x) => x match {
        case n: Nat => Some(s(f).mapSuccess(DepApply[NatKind](_, n) ))
        case dt: DataType => Some(s(f).mapSuccess(DepApply[DataKind](_, dt) ))
      }
      case Literal(_) => None
      case TypedExpr(e, t) => Some(s(e).mapSuccess(TypedExpr(_, t)))
      case ff: primitives.ForeignFunction => None
      case p: Primitive => None
    }

  // applies s to all direct subexpressions
  case class all(s: Strategy[Lift]) extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case Apply(f, e) => s(f).flatMapSuccess(a => s(e).mapSuccess(b => Apply(a, b) ) )

      case x => traverseSingleSubexpression(s)(x) match {
        case Some(r) => r
        case None => Success(x)
      }
    }
  }

  case class oneHandlingState(carryOverState: Boolean, s: Strategy[Lift]) extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case Apply(f, e) => s(f) match {
        case Success(x: Lift) => Success(Apply(x, e))
        case Failure(state) => if (carryOverState)
          state(e).mapSuccess(Apply(f, _) ) else
          s(e).mapSuccess(Apply(f, _))
      }
      case x => traverseSingleSubexpression(s)(x) match {
        case Some(r) => r
        case None => Failure(s)
      }
    }
  }

  // applies s to one direct subexpression
  object one {
    def apply(s: Strategy[Lift]): oneHandlingState = oneHandlingState(carryOverState = false,s)
  }

  object oneWithState {
    def apply(s: Strategy[Lift]): oneHandlingState = oneHandlingState(carryOverState = true,s)
  }

  // applies s to at least one direct subexpression and as many as possible
  case class some(s: Strategy[Lift]) extends Strategy[Lift]  {
    def apply(e: Lift): RewriteResult[Lift] = e match {
      case Apply(f, e) => (s(f), s(e)) match {
        case (Failure(_), Failure(_)) => Failure(s)
        case (x, y) => Success(Apply(x.getProgramOrElse(f), y.getProgramOrElse(e)))
      }
      case x => traverseSingleSubexpression(s)(x) match {
        case Some(r) => r
        case None => Failure(s)
      }
    }
  }


  // generic traversal strategies

  case class oncetd(s: Strategy[Lift]) extends Strategy[Lift] {
    def apply(e: Lift): RewriteResult[Lift] = (s <+ one(oncetd(s))) (e)
  }

  def topdown: Strategy[Lift] => Strategy[Lift] = s => s `;` (e => all(topdown(s))(e))

  def bottomup: Strategy[Lift] => Strategy[Lift] = s => ((e: Lift) => all(bottomup(s))(e)) `;` s

  def downup: Strategy[Lift] => Strategy[Lift] = s => s `;` (e => (all(downup(s)) `;` s)(e))

  def downup2: Strategy[Lift] => Strategy[Lift] => Strategy[Lift] = s1 => s2 => s1 `;` (e => (all(downup2(s1)(s2)) `;` s2)(e))


  def oncebu: Strategy[Lift] => Strategy[Lift] = s => ((e: Lift) => one(oncebu(s))(e)) <+ s

  def alltd: Strategy[Lift] => Strategy[Lift] = s => s <+ (e => all(alltd(s))(e))

  def tryAll: Strategy[Lift] => Strategy[Lift] = s => ((e: Lift) => all(tryAll(`try`(s)))(e)) `;` `try`(s)

  def sometd: Strategy[Lift] => Strategy[Lift] = s => s <+ (e => some(sometd(s))(e))

  def somebu: Strategy[Lift] => Strategy[Lift] = s => ((e: Lift) => some(somebu(s))(e)) <+ s

  def position(n: Int): Strategy[Lift] => Strategy[Lift] = s => if(n <= 0) s else oneWithState(position(n-1)(s))

  def skip(n: Int): Strategy[Lift] => Strategy[Lift] = s => e => s(e) match {
    case Failure(a) =>
      oneWithState(skip(n)(a))(e)
    case Success(r) if n <= 0 =>
      Success(r)
    case Success(_) if n > 0 =>
      oneWithState(skip(n - 1)(s))(e)
  }

  // todo figure out whats wrong here
  def innermost: Strategy[Lift] => Strategy[Lift] = s => bottomup(`try`(e => (s `;` innermost(s))(e)))
}
