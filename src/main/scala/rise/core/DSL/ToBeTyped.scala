package rise.core.DSL

import rise.core.semantics.Data
import rise.core.traversal.{Continue, Stop}
import rise.core.types.{Flags, TypePlaceholder}
import rise.core.{Expr, Literal, Primitive, TypeAnnotation, TypeAssertion, Traverse, traversal}

final case class ToBeTyped[+T <: Expr](private val e: T) {
  def toExpr: Expr = infer(e)
  def toUntypedExpr: Expr = Traverse(e, new Traverse.Traversal {
    override def data : Data => Data = id
    override def primitive : Primitive => Expr = {
      case Opaque(x, t) => expr(x)
      case tl@TopLevel(x, t) => expr(x)
      case TypeAnnotation(e, t) => expr(e)
      case TypeAssertion(e, t) => expr(e)
      case p => super.primitive(p.setType(TypePlaceholder))
    }
  })
  def >>=[X <: Expr](f: T => ToBeTyped[X]): ToBeTyped[X] = f(e)
}
