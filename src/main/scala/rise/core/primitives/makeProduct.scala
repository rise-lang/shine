package rise.core.primitives
import rise.core.DSL._
import rise.core.DSL.Type._
import rise.core._
import rise.core.types._
import arithexpr.arithmetic._
final case class makeProduct(n: Int) extends Builder {
  override def toString: String = "makeProduct"
  override def primitive: rise.core.Primitive = makeProduct.Primitive(n)()
  override def apply: ToBeTyped[rise.core.Primitive] = toBeTyped(makeProduct.Primitive(n)())
  override def unapply(arg: Expr): Boolean = arg match {
    case _: Primitive => true
    case _ => false
  }
}
object makeProduct {
  private final case class Primitive(n: Int)(override val t: Type = TypePlaceholder) extends rise.core.Primitive {
    override val name: String = "makeProduct"
    override def setType(ty: Type): Primitive = Primitive(n)(ty)
    override def typeScheme: Type = {
      val dts = Seq.fill(n)(DataTypeIdentifier(freshName("dt")))
      dts.foldRight(ProductType(dts): Type)({
        case (lhsT, rhsT) => lhsT ->: rhsT
      })
    }
    override def primEq(obj: rise.core.Primitive): Boolean = obj match {
      case p: Primitive =>
        p.n == n && true
      case _ =>
        false
    }
  }
  def unapply(arg: rise.core.Expr): Option[Int] = arg match {
    case p: Primitive =>
      Some(p.n)
    case _ =>
      None
  }
}
