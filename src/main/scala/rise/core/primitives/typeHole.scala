package rise.core.primitives

import rise.core.DSL.ToBeTyped
import rise.core.DSL.Type._
import rise.core._
import rise.core.types._

final case class typeHole(msg: String = "") extends Builder {
  override def apply: ToBeTyped[Primitive] = ToBeTyped(typeHole.Primitive(msg)())

  override def primitive: Primitive = typeHole.Primitive(msg)()

  override def unapply(arg: Expr): Boolean = arg match {
    case _: typeHole.Primitive => true
    case _ => false
  }
}

object typeHole {
  private final case class Primitive(msg: String)
                                    (override val t: ExprType = TypePlaceholder)
    extends rise.core.Primitive
  {
    override def name: String = s"printType($msg)"
    override def primEq(obj: rise.core.Primitive): Boolean = obj.getClass == getClass
    override def setType(t: ExprType): Primitive = Primitive(msg)(t)
    override def typeScheme: ExprType = impl{ t: TypeIdentifier => t }
  }

  def unapply(arg: Expr): Option[String] = arg match {
    case p: typeHole.Primitive => Some(p.msg)
    case _ => None
  }
}
