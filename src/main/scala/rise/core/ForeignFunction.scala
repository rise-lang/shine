package rise.core

import parser.Span
import rise.core.types.Type

case class ForeignFunction(decl: ForeignFunction.Decl)(override val t: Type, override val span: Option[Span]=None)
    extends Primitive {
  override def typeScheme: Type = t
  override def setType(t: Type): ForeignFunction = ForeignFunction(decl)(t, span)
  override val name: String = decl.name
}

object ForeignFunction {
  case class Decl(name: String, definition: Option[Def])
  case class Def(params: Seq[String], body: String)
}
