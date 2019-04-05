package elevate.core.strategies

import elevate.core._

import lift.core.{Expr, traversal => lt}

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
}