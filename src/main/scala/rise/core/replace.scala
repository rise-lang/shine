package rise.core

import rise.core.traverse._

object replace {
  def exprInExpr(expression: Expr, `for`: Expr, in: Expr): Expr = {
    object Visitor extends PureExprTraversal {
      override def expr: Expr => Pure[Expr] = e =>
        if (`for` =~= e) return_(expression) else super.expr(e)
    }
    Visitor.expr(in).unwrap
  }
}
