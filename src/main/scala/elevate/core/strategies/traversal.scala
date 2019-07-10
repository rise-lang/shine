package elevate.core.strategies

import elevate.core._
import _root_.lift.core.types.{DataType, DataTypeIdentifier}
import _root_.lift.core.{Apply, DepApply, DepLambda, Expr, Identifier, Index, Lambda, Literal, Nat, NatDepApply, NatDepLambda, NatExpr, NatIdentifier, Primitive, TypeDepApply, TypeDepLambda, TypedExpr, primitives, traversal => lt}

object traversal {

  // generic one-level traversal operators

  private def inject(generator: Expr => Expr, test: RewriteResult): RewriteResult = {
    test match {
      case Success(e) => Success(generator(e))
      case f:Failure => f
    }
  }

  private def traverseSingleSubexpression: Strategy => Expr => Option[RewriteResult] =
    s => {
      case Identifier(_) => None
      case Lambda(x, e) => Some(inject(Lambda(x, _), s(e)))
      case DepLambda(x, e) => x match {
        case n: NatIdentifier => Some(inject(NatDepLambda(n, _), s(e)))
        case dt: DataTypeIdentifier => Some(inject(TypeDepLambda(dt, _), s(e)))
      }
      case DepApply(f, x) => x match {
        case n: Nat => Some(inject(NatDepApply(_, n), s(f)))
        case dt: DataType => Some(inject(TypeDepApply(_, dt), s(f)))
      }
      case Literal(_) => None
      case Index(_, _) => None
      case NatExpr(_) => None
      case TypedExpr(e, t) => Some(inject(TypedExpr(_, t), s(e)))
      case ff: primitives.ForeignFunction => None
      case p: Primitive => None
    }

  // applies s to all direct subexpressions
  def all: Strategy => Strategy = s => {
    case Apply(f, e) => (s(f), s(e)) match {
      case (Success(a), Success(b)) => Success(Apply(a,b))
      case _ => Failure(s)
    }
    case x => traverseSingleSubexpression(s)(x) match {
      case Some(r) => r
      case None => Success(x)
    }
  }

  private def oneHandlingState(carryOverState: Boolean) : Strategy => Strategy = s => {
    case Apply(f, e) => s(f) match {
        case Success(x) => Success(Apply(x,e))
        case Failure(state) => if(carryOverState)
          inject(Apply(f, _), state(e)) else
          inject(Apply(f, _), s(e))
      }
    case x => traverseSingleSubexpression(s)(x) match {
      case Some(r) => r
      case None => Failure(s)
    }
  }

  // applies s to one direct subexpression
  def one: Strategy => Strategy = oneHandlingState(false)
  def oneWithState: Strategy => Strategy = oneHandlingState(true)

  // applies s to at least one direct subexpression and as many as possible
  def some: Strategy => Strategy = s => {
    case Apply(f, e) => (s(f), s(e)) match {
      case (Failure(_), Failure(_)) => Failure(s)
      case (x, y) => Success(Apply(x.getExprOrElse(f), y.getExprOrElse(e)))
    }
    case x => traverseSingleSubexpression(s)(x) match {
      case Some(r) => r
      case None => Failure(s)
    }
  }

  // generic traversal strategies

  def topdown: Strategy => Strategy = s => s `;` (e => all(topdown(s))(e))

  def bottomup: Strategy => Strategy = s => ((e: Expr) => all(bottomup(s))(e)) `;` s

  def downup: Strategy => Strategy = s => s `;` (e => (all(downup(s)) `;` s)(e))

  def downup2: Strategy => Strategy => Strategy = s1 => s2 => s1 `;` (e => (all(downup2(s1)(s2)) `;` s2)(e))

  def oncetd: Strategy => Strategy = s => s <+ (e => one(oncetd(s))(e))

  def oncebu: Strategy => Strategy = s => ((e: Expr) => one(oncebu(s))(e)) <+ s

  def alltd: Strategy => Strategy = s => s <+ (e => all(alltd(s))(e))

  def sometd: Strategy => Strategy = s => s <+ (e => some(sometd(s))(e))

  def somebu: Strategy => Strategy = s => ((e: Expr) => some(somebu(s))(e)) <+ s

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
}
