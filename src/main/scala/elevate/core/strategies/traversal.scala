package elevate.core.strategies

import elevate.core._
import lift.arithmetic.NamedVar
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
        case lt.Continue(_, _) => throw NotFound
      }
    }
  def find: Strategy => TraversalStrategy =
    s => e => {
      mayApply(s)(e) match {
        case None => Continue(e, find(s))
        case Some(r) => Stop(r)
      }
    }

  def position(n: Int): Strategy => TraversalStrategy =
    s => e => {
      if (n <= 0) {
        Stop(s(e))
      } else {
        Continue(e, position(n - 1)(s))
      }
    }

  def drop(n: Int): Strategy => TraversalStrategy =
    s => e => {
      mayApply(s)(e) match {
        case None => Continue(e, drop(n)(s))
        case Some(r) => if (n <= 0) {
          Stop(r)
        } else {
          Continue(e, drop(n - 1)(s))
        }
      }
    }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // generic one-level traversal operators
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  private def traverseSingleSubexpression: Strategy => Strategy =
    s => {
    case i: Identifier => i
    case Lambda(x, e) => Lambda(x, s(e))
    case DepLambda(x, e) => x match {
      case n: NatIdentifier => NatDepLambda(n, s(e))
      case dt: DataTypeIdentifier => TypeDepLambda(dt, s(e))
    }
    case DepApply(f, x) => x match {
      case n: Nat => NatDepApply(s(f), n)
      case dt: DataType => TypeDepApply(s(f), dt)
    }
    case l: Literal => l
    case idx: Index => idx
    case ne: NatExpr => ne
    case TypedExpr(e, t) => TypedExpr(s(e), t)
    case ff: primitives.ForeignFunction => ff
    case p:Primitive => p
    }

  private def containsSubexpression(e: Expr): Boolean = {
    e match {
      case Identifier(_) => false
      case Lambda(_, _) => true
      case Apply(_, _) => true
      case DepLambda(_, _) => true
      case DepApply(_, _) => true
      case Literal(_) => false
      case Index(_, _) => false
      case NatExpr(_) => false
      case TypedExpr(_, _) => true
      case ff: primitives.ForeignFunction => false
      case p: Primitive => false
    }
  }

  // applies s to all direct subexpressions
  def all: Strategy => Strategy = s => {
    case Apply(f, e) => Apply(s(f), s(e))
    case x => traverseSingleSubexpression(s)(x)
  }

  // applies s to one direct subexpression
  def one: Strategy => Strategy = s => {
    case Apply(f, e) => mayApply(s)(f) match {
        case Some(x) => Apply(x,e)
        case _ => Apply(f, s(e))
      }
    case x if containsSubexpression(x) => traverseSingleSubexpression(s)(x)
    case _ => throw NotFound
  }

  // applies s to at least one direct subexpression and as many as possible
  def some: Strategy => Strategy = s => {
    case Apply(f, e) => (mayApply(s)(f), mayApply(s)(e)) match {
      case (None, None) => throw NotFound
      case (x, y) => Apply(x.getOrElse(f), y.getOrElse(e))
    }
    case x if containsSubexpression(x) => traverseSingleSubexpression(s)(x)
    case _ => throw NotFound
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

  // todo figure out whats wrong here
  //def innermost: Strategy => Strategy = s => bottomup(`try`(s `;` innermost(s)))
}