package rise.core

import rise.core.types.{Type, TypeException, TypePlaceholder}

case class TypeAnnotation(e: Expr, annotation: Type) extends Primitive {
  override val t: Type = TypePlaceholder
  override def typeScheme: Type =
    throw TypeException("cannot get the type scheme of an annotated Expr")
  override def setType(t: Type): TypeAnnotation =
    throw TypeException("cannot set the type of an annotated Expr")
  override def name: String = s"$e: $annotation"
}
