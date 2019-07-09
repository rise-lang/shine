package elevate.core.strategies

import elevate.core._
import lift.core.types.{DataType, DataTypeIdentifier}
import lift.core.{Apply, DepApply, DepLambda, Expr, Identifier, Index, Lambda, Literal, Nat, NatDepApply, NatDepLambda, NatExpr, NatIdentifier, Primitive, TypeDepApply, TypeDepLambda, TypedExpr, primitives, traversal => lt}

object traversal {
  sealed trait Result
  case class Stop(e: Expr) extends Result
  case class Continue(e: Expr, ts: TraversalStrategy) extends Result
  type TraversalStrategy = Expr => Result

  case class Visitor(ts: TraversalStrategy) extends lt.Visitor {
    override def apply(e: Expr): lt.Result[Expr] = {
      ts(e) match {
        case Stop(r) => lt.Stop(r)
        case Continue(ke, kts) => lt.Continue(ke, Visitor(kts))
      }
    }
  }

  def depthFirst: TraversalStrategy => Strategy =
    ts => expr => {
      lt.DepthFirstGlobalResult(expr, Visitor(ts)) match {
        case lt.Stop(r) => r
        case lt.Continue(_, _) => throw NotApplicable(???)
      }
    }

  @deprecated
  def find: Strategy => TraversalStrategy =
    s => e => {
      mayApply(s)(e) match {
        case Failure(_) => Continue(e, find(s))
        case Success(r) => Stop(r)
      }
    }

  @deprecated
  def positionOld(n: Int): Strategy => TraversalStrategy =
    s => e => {
      if (n <= 0) {
        Stop(s(e))
      } else {
        Continue(e, positionOld(n - 1)(s))
      }
    }

  @deprecated
  def dropOld(n: Int): Strategy => TraversalStrategy =
    s => e => {
      mayApply(s)(e) match {
        case Failure(_) => Continue(e, dropOld(n)(s))
        case Success(r) => if (n <= 0) {
          Stop(r)
        } else {
          Continue(e, dropOld(n - 1)(s))
        }
      }
    }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // generic one-level traversal operators
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  private def traverseSingleSubexpression: Strategy => Expr => Option[Expr] =
    s => {
      case Identifier(_) => None
      case Lambda(x, e) => Some(Lambda(x, s(e)))
      case DepLambda(x, e) => x match {
        case n: NatIdentifier => Some(NatDepLambda(n, s(e)))
        case dt: DataTypeIdentifier => Some(TypeDepLambda(dt, s(e)))
      }
      case DepApply(f, x) => x match {
        case n: Nat => Some(NatDepApply(s(f), n))
        case dt: DataType => Some(TypeDepApply(s(f), dt))
      }
      case Literal(_) => None
      case Index(_, _) => None
      case NatExpr(_) => None
      case TypedExpr(e, t) => Some(TypedExpr(s(e), t))
      case ff: primitives.ForeignFunction => None
      case p: Primitive => None
    }

  // applies s to all direct subexpressions
  def all: Strategy => Strategy = s => {
    case Apply(f, e) => Apply(s(f), s(e))
    case x => traverseSingleSubexpression(s)(x) match {
      case Some(e) => e
      case None => x
    }
  }

  private def oneHandlingState(carryOverState: Boolean) : Strategy => Strategy = s => {
    case Apply(f, e) => mayApply(s)(f) match {
        case Success(x) => Apply(x,e)
        case Failure(state) => if(carryOverState)
          Apply(f, state(e)) else Apply(f, s(e))
      }
    case x => traverseSingleSubexpression(s)(x) match {
      case Some(e) => e
      case None => throw NotApplicable(s)
    }
  }

  // applies s to one direct subexpression
  def one: Strategy => Strategy = oneHandlingState(false)
  def oneWithState: Strategy => Strategy = oneHandlingState(true)

  // applies s to at least one direct subexpression and as many as possible
  def some: Strategy => Strategy = s => {
    case Apply(f, e) => (mayApply(s)(f), mayApply(s)(e)) match {
      case (Failure(_), Failure(_)) => throw NotApplicable(s)
      case (x, y) => Apply(x.getExprOrElse(f), y.getExprOrElse(e))
    }
    case x => traverseSingleSubexpression(s)(x) match {
      case Some(e) => e
      case None => throw NotApplicable(s)
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // generic traversal strategies
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

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

  def skip(n: Int): Strategy => Strategy = s => e => mayApply(s)(e) match {
    case Failure(a) =>
      oneWithState(skip(n)(a))(e)
    case Success(r) if n <= 0 =>
      r
    case Success(_) if n > 0 =>
      oneWithState(skip(n-1)(s))(e)
  }

  // todo figure out whats wrong here
  def innermost: Strategy => Strategy = s => bottomup(`try`(e => (s `;` innermost(s))(e)))
}