package rise.core.DSL

import rise.core.{Expr, Primitive}
import rise.core.types.Type

final case class Opaque(e: Expr, override val t: Type) extends Primitive {
  // TODO: Ignored by alpha equivalence, remove when taking out of primitives
  override def primEq(obj: Primitive): Boolean = obj.getClass == getClass
  override def name: String = s"{Opaque Expr: $t}"
}
