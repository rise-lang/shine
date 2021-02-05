package rise.core.DSL

import rise.core.Traverse.Pure
import rise.core.semantics.Data
import rise.core.types.{TypePlaceholder}
import rise.core.{Expr, Primitive, Traverse, TypeAnnotation, TypeAssertion}

final case class ToBeTyped[+T <: Expr](private val e: T) {
  def toExpr: Expr = infer(e)
  def toUntypedExpr: Expr = new Traverse.PureTraversal {
    override def data : Data => Pure[Data] = return_
    override def primitive : Primitive => Pure[Expr] = {
      case Opaque(x, t) => expr(x)
      case tl@TopLevel(x, t) => expr(x)
      case TypeAnnotation(e, t) => expr(e)
      case TypeAssertion(e, t) => expr(e)
      case p => super.primitive(p.setType(TypePlaceholder))
    }
  }.expr(e).unwrap
  def >>=[X <: Expr](f: T => ToBeTyped[X]): ToBeTyped[X] = f(e)
}
