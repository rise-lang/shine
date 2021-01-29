package rise.core

import rise.core.traversal.{Continue, Result, Stop}

object replace {
  def exprInExpr(expr: Expr, `for`: Expr, in: Expr): Expr = {
    object Visitor extends traversal.Visitor {
      override def visitExpr(e: Expr): Result[Expr] = {
        if (`for` == e) {
          Stop(expr)
        } else {
          Continue(e, this)
        }
      }
    }

    traversal.DepthFirstLocalResult(in, Visitor)
  }
}
