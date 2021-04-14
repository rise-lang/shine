package rise.core

import rise.core.types.{Type, TypeException, TypePlaceholder}

case class TypeAssertion(e: Expr, assertion: Type) extends Primitive {
  // TODO: Ignored by alpha equivalence, remove when taking out of primitives
  override def primEq(obj: Primitive): Boolean = obj.getClass == getClass
  override val t: Type = TypePlaceholder
  override def typeScheme: Type =
    throw TypeException("cannot get the type scheme of an annotated Expr")
  override def setType(t: Type): TypeAnnotation =
    throw TypeException("cannot set the type of an annotated Expr")
  override def name: String = s"$e !: $assertion"
}
