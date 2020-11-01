package rise.core.dsl

import rise.core.exprs.{Expr, Literal, TypeAnnotation, TypeAssertion}
import rise.core.types.TypePlaceholder
import rise.core.util.traversal
import rise.core.util.traversal.{Continue, Stop}

final case class ToBeTyped[+T <: Expr](private val e: T) {
  def toExpr: Expr = infer(e)
  def toUntypedExpr: Expr = traversal.DepthFirstLocalResult(e, new traversal.Visitor {
    override def visitExpr(e: Expr): traversal.Result[Expr] = e match {
      case Opaque(x, _) => Continue(x, this)
      case TopLevel(x, _) => Continue(x, this)
      case Literal(_) => Stop(e)
      case TypeAnnotation(e, _) => Continue(e, this)
      case TypeAssertion(e, _) => Continue(e, this)
      case _ => Continue(e.setType(TypePlaceholder), this)
    }
  })
  def >>=[X <: Expr](f: T => ToBeTyped[X]): ToBeTyped[X] = f(e)
}
