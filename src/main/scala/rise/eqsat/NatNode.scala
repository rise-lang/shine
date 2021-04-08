package rise.eqsat

case class Nat(node: NatNode[Nat]) {
  override def toString: String = node.toString
}

object Nat {
  import arithexpr.arithmetic._

  def fromNamed(n: rise.core.types.Nat, bound: Expr.Bound = Expr.Bound.empty): Nat = {
    Nat(n match {
      case i: rise.core.types.NatIdentifier => NatVar(bound.indexOf(i))
      case Cst(c) => NatCst(c)
      case _ => ???
    })
  }

  def toNamed(n: Nat, bound: Expr.Bound = Expr.Bound.empty): rise.core.types.Nat = {
    n.node match {
      case NatVar(index) => bound.nat(index)
      case NatCst(value) => Cst(value)
    }
  }
}

sealed trait NatNode[+N] {
  def map[O](f: N => O): NatNode[O] = this match {
    case v: NatVar => v
    case c: NatCst => c
  }
  def nats(): Iterator[N] = this match {
    case _: NatVar => Iterator()
    case _: NatCst => Iterator()
  }
  def natCount(): Int = nats().size
}
case class NatVar(index: Int) extends NatNode[Nothing] {
  override def toString: String = s"%n$index"
}
case class NatCst(value: Long) extends NatNode[Nothing] {
  override def toString: String = value.toString
}

sealed trait NatPattern
case class NatPatternNode(n: NatNode[NatPattern]) extends NatPattern
case class NatPatternVar(index: Int) extends NatPattern
case object NatPatternAny extends NatPattern

object NatPattern {
  def fromNat(n: Nat): NatPattern = ???
}