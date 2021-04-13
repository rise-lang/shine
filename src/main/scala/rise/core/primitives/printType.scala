package rise.core.primitives

import rise.core.DSL.ToBeTyped
import rise.core.DSL.Type._
import rise.core._
import rise.core.types._

final case class printType(msg: String = "") extends Builder {
  override def apply: ToBeTyped[Primitive] = ToBeTyped(printType.Primitive(msg)())

  override def primitive: Primitive = printType.Primitive(msg)()

  override def unapply(arg: Expr): Boolean = arg match {
    case _: printType.Primitive => true
    case _ => false
  }
}

object printType {
  private final case class Primitive(msg: String)
                            (override val t: Type = TypePlaceholder)
    extends rise.core.Primitive
  {
    override def name: String = s"printType($msg)"

    override def setType(t: Type): Primitive = Primitive(msg)(t)

    override def typeScheme: Type = impl{ t: TypeIdentifier => t ->: t }
  }

  def unapply(arg: Expr): Option[String] = arg match {
    case p: printType.Primitive => Some(p.msg)
    case _ => None
  }
}
