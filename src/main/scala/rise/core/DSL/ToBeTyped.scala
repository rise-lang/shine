package rise.core.DSL

import rise.core.traverse.Pure
import rise.core.types._
import rise.core._

final case class ToBeTyped[+T <: Expr](private val e: T) {
  def toExpr: Expr = infer(e)
  def toUntypedExpr: Expr = new traverse.PureTraversal {
    override def expr : Expr => Pure[Expr] = {
      case l@Literal(_) => return_(l : Expr)
      case Opaque(x, t) => expr(x)
      case tl@TopLevel(x, t) => expr(x)
      case TypeAnnotation(e, t) => expr(e)
      case TypeAssertion(e, t) => expr(e)
      case p => super.`expr`(p.setType(TypePlaceholder))
    }
  }.expr(e).unwrap
  def >>=[X <: Expr](f: T => ToBeTyped[X]): ToBeTyped[X] = f(e)
}
