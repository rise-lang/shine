package elevate.core.strategies

import elevate.core._
import elevate.core.strategies.basic._
import lift.core.types._
import lift.core._

object traversal {

  // generic one-level traversal operators

  private def traverseSingleSubexpression: Strategy[Expr] => Program => Option[RewriteResult[Expr]] =
    s => {
      case Identifier(_) => None
      case Lambda(x, e) => Some(s(e).mapSuccess({case y: Expr => Lambda(x, y)}))
      case DepLambda(x, e) => x match {
        case n: NatIdentifier => Some(s(e).mapSuccess({case y: Expr => DepLambda[NatKind](n, y)}))
        case dt: DataTypeIdentifier => Some(s(e).mapSuccess({case y : Expr => DepLambda[DataKind](dt, y)}))
      }
      case DepApply(f, x) => x match {
        case n: Nat => Some(s(f).mapSuccess({case y:Expr => DepApply[NatKind](y, n)}))
        case dt: DataType => Some(s(f).mapSuccess({case y:Expr => DepApply[DataKind](y, dt)}))
      }
      case Literal(_) => None
      case Index(_, _) => None
      case NatExpr(_) => None
      case TypedExpr(e, t) => Some(s(e).mapSuccess({case y:Expr => TypedExpr(y, t)}))
      case ff: primitives.ForeignFunction => None
      case p: Primitive => None
    }

  // applies s to all direct subexpressions
  case class all[T <: Program](s: Strategy[Expr]) extends Strategy[Expr] {
    def apply(e: Expr): RewriteResult[Expr] = e match {
      case Apply(f, e) => s(f).flatMapSuccess({case a:Expr => s(e).mapSuccess({case b:Expr => Apply(a, b)})})

      case x => traverseSingleSubexpression(s)(x) match {
        case Some(r) => r
        case None => Success(x)
      }
    }
  }

  case class oneHandlingState[T <: Program](carryOverState: Boolean, s: Strategy[Expr]) extends Strategy[Expr] {
    def apply(e: Expr): RewriteResult[Expr] = e match {
      case Apply(f, e) => s(f) match {
        case Success(x: Expr) => Success(Apply(x, e))
        case Failure(state) => if (carryOverState)
          state(e).mapSuccess({ case y: Expr => Apply(f, y) }) else
          s(e).mapSuccess({ case y: Expr => Apply(f, y) })
      }
      case x => traverseSingleSubexpression(s)(x) match {
        case Some(r) => r
        case None => Failure(s)
      }
    }
  }

  // applies s to one direct subexpression
  object one {
    def apply[T <: Program](s: Strategy[Expr]): oneHandlingState[T] = oneHandlingState[T](false,s)
  }

  object oneWithState {
    def apply[T <: Program](s: Strategy[Expr]): oneHandlingState[T] = oneHandlingState[T](true,s)
  }

  // applies s to at least one direct subexpression and as many as possible
  case class some[T <: Program](s: Strategy[Expr]) extends Strategy[Expr]  {
    def apply(e: Expr): RewriteResult[Expr] = e match {
      case Apply(f, e) => (s(f), s(e)) match {
        case (Failure(_), Failure(_)) => Failure(s)
        case (x, y) => Success(Apply(x.getProgramOrElse[Expr](f), y.getProgramOrElse[Expr](e)))
      }
      case x => traverseSingleSubexpression(s)(x) match {
        case Some(r) => r
        case None => Failure(s)
      }
    }
  }


  // generic traversal strategies

  case class oncetd[T <: Program](s: Strategy[Expr]) extends Strategy[Expr] {
    def apply(e: Expr): RewriteResult[Expr] = (s <+ one(oncetd(s)))(e)
  }
  /*
  def topdown: Strategy => Strategy = s => s `;` (e => all(topdown(s))(e))

  def bottomup: Strategy => Strategy = s => ((e: Program) => all(bottomup(s))(e)) `;` s

  def downup: Strategy => Strategy = s => s `;` (e => (all(downup(s)) `;` s)(e))

  def downup2: Strategy => Strategy => Strategy = s1 => s2 => s1 `;` (e => (all(downup2(s1)(s2)) `;` s2)(e))


  def oncebu: Strategy => Strategy = s => ((e: Program) => one(oncebu(s))(e)) <+ s

  def alltd: Strategy => Strategy = s => s <+ (e => all(alltd(s))(e))

  def tryAll: Strategy => Strategy = s => ((e: Program) => all(tryAll(`try`(s)))(e)) `;` `try`(s)

  def sometd: Strategy => Strategy = s => s <+ (e => some(sometd(s))(e))

  def somebu: Strategy => Strategy = s => ((e: Program) => some(somebu(s))(e)) <+ s

  def position(n: Int): Strategy => Strategy = s => if(n <= 0) s else oneWithState(position(n-1)(s))

  def skip(n: Int): Strategy => Strategy = s => e => s(e) match {
    case Failure(a) =>
      oneWithState(skip(n)(a))(e)
    case Success(r) if n <= 0 =>
      Success(r)
    case Success(_) if n > 0 =>
      oneWithState(skip(n - 1)(s))(e)
  }

  // todo figure out whats wrong here
  def innermost: Strategy => Strategy = s => bottomup(`try`(e => (s `;` innermost(s))(e)))

   */
}
