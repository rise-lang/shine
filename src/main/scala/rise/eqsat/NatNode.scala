package rise.eqsat

case class Nat(node: NatNode[Nat]) {
  override def toString: String = node.toString
}

object Nat {
  import arithexpr.arithmetic._

  def fromNamed(n: rise.core.types.Nat, bound: Expr.Bound): Nat = {
    Nat(n match {
      case i: rise.core.types.NatIdentifier => NatVar(bound.indexOf(i))
      case Cst(c) => NatCst(c)
      case _ => ???
    })
  }

  def toNamed(n: Nat, bound: Expr.Bound): rise.core.types.Nat = {
    n.node match {
      case NatVar(index) => bound.nat(index)
      case NatCst(value) => Cst(value)
    }
  }
}

sealed trait NatNode[+N]
case class NatVar(index: Int) extends NatNode[Nothing]
case class NatCst(value: Long) extends NatNode[Nothing]

sealed trait NatPattern
case class NatPatternNode(n: NatNode[NatPattern]) extends NatPattern
case class NatPatternVar(index: Int) extends NatPattern
