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
      case Sum(Nil) => NatCst(0)
      case Sum(t +: ts) => ts.foldRight(fromNamed(t, bound)) { case (t, acc) =>
        Nat(NatAdd(fromNamed(t, bound), acc))
      }.node
      case Prod(Nil) => NatCst(1)
      case Prod(t +: ts) => ts.foldRight(fromNamed(t, bound)) { case (t, acc) =>
        Nat(NatMul(fromNamed(t, bound), acc))
      }.node
      case _ => ???
    })
  }

  def toNamed(n: Nat, bound: Expr.Bound = Expr.Bound.empty): rise.core.types.Nat = {
    n.node match {
      case NatVar(index) => bound.nat(index)
      case NatCst(value) => Cst(value)
      case NatAdd(a, b) => toNamed(a, bound) + toNamed(b, bound)
      case NatMul(a, b) => toNamed(a, bound) * toNamed(b, bound)
    }
  }
}

sealed trait NatNode[+N] {
  def map[O](f: N => O): NatNode[O] = this match {
    case v: NatVar => v
    case c: NatCst => c
    case NatAdd(a, b) => NatAdd(f(a), f(b))
    case NatMul(a, b) => NatMul(f(a), f(b))
  }
  def nats(): Iterator[N] = this match {
    case _: NatVar => Iterator()
    case _: NatCst => Iterator()
    case NatAdd(a, b) => Iterator(a, b)
    case NatMul(a, b) => Iterator(a, b)
  }
  def natCount(): Int = nats().size
}
case class NatVar(index: Int) extends NatNode[Nothing] {
  override def toString: String = s"%n$index"
}
case class NatCst(value: Long) extends NatNode[Nothing] {
  override def toString: String = value.toString
}
case class NatAdd[N](a: N, b: N) extends NatNode[N] {
  override def toString: String = s"($a + $b)"
}
case class NatMul[N](a: N, b: N) extends NatNode[N] {
  override def toString: String = s"($a * $b)"
}

sealed trait NatPattern
case class NatPatternNode(n: NatNode[NatPattern]) extends NatPattern {
  override def toString: String = n.toString
}
case class NatPatternVar(index: Int) extends NatPattern {
  override def toString: String = s"?n$index"
}
case object NatPatternAny extends NatPattern {
  override def toString: String = "?n"
}

object NatPattern {
  def fromNat(n: Nat): NatPattern = ???
}