// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package rise.Cuda.primitives
import rise.core.DSL._
import rise.core.DSL.Type._
import rise.core._
import rise.core.types._
import rise.core.types.DataType._
import arithexpr.arithmetic._
object asFragment extends Builder {
  private final case class Primitive()(override val t: ExprType = TypePlaceholder) extends rise.core.Primitive {
    override val name: String = "asFragment"
    override def setType(ty: ExprType): Primitive = Primitive()(ty)
    override def primEq(obj: rise.core.Primitive): Boolean = obj.getClass == getClass
    override def typeScheme: ExprType = impl { (n: Nat) => impl { (m: Nat) => impl { (k: Nat) => impl { (s: DataType) => impl { (f: Fragment) => impl { (l: MatrixLayout) => ArrayType(n, ArrayType(m, s)) ->: FragmentType(n, m, k, s, f, l) } } } } } }
  }
  override def toString: String = "asFragment"
  override def primitive: rise.core.Primitive = Primitive()()
  override def apply: ToBeTyped[rise.core.Primitive] = toBeTyped(Primitive()())
  override def unapply(arg: Expr): Boolean = arg match {
    case _: Primitive => true
    case _ => false
  }
}
