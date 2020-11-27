package rise.core.DSL

import rise.core.{Expr, Primitive}
import rise.core.types.Type

final case class Opaque(e: Expr, override val t: Type) extends Primitive {
  override def name: String = s"{Opaque Expr: $t}"
}
