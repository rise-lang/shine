package rise.eqsat

import rise.core.types.NatIdentifier
import rise.core.{types => rct}

/** A Rise arithmetic expression based on DeBruijn indexing */
case class Nat(node: NatNode[Nat]) {
  override def toString: String = node.toString

  /** Shifts DeBruijn indices up or down if they are >= cutoff */
  def shifted(shift: Nat.Shift, cutoff: Nat.Shift): Nat = {
    Nat(node match {
      case NatVar(index) =>
        val delta = if (index >= cutoff) shift else 0
        NatVar(index + delta)
      case NatCst(value) =>
        NatCst(value)
      case NatAdd(a, b) =>
        NatAdd(a.shifted(shift, cutoff), b.shifted(shift, cutoff))
      case NatMul(a, b) =>
        NatMul(a.shifted(shift, cutoff), b.shifted(shift, cutoff))
    })
  }
}

object Nat {
  import arithexpr.arithmetic._

  /** Shift nat indices */
  type Shift = Int

  def fromNamed(n: rct.Nat, bound: Expr.Bound = Expr.Bound.empty): Nat =
    fromNamedGeneric(n, bound.indexOf)
  def fromNamedGeneric(n: rct.Nat, indexOf: rct.NatIdentifier => Int): Nat = {
    Nat(n match {
      case i: rct.NatIdentifier => NatVar(indexOf(i))
      case Cst(c) => NatCst(c)
      case Sum(Nil) => NatCst(0)
      case Sum(t +: ts) => ts.foldRight(fromNamedGeneric(t, indexOf)) { case (t, acc) =>
        Nat(NatAdd(fromNamedGeneric(t, indexOf), acc))
      }.node
      case Prod(Nil) => NatCst(1)
      case Prod(t +: ts) => ts.foldRight(fromNamedGeneric(t, indexOf)) { case (t, acc) =>
        Nat(NatMul(fromNamedGeneric(t, indexOf), acc))
      }.node
      case _ => ???
    })
  }

  def toNamed(n: Nat, bound: Expr.Bound = Expr.Bound.empty): rct.Nat =
    toNamedGeneric(n, bound.nat(_))
  def toNamedGeneric(n: Nat, nameOf: Int => rct.NatIdentifier): rct.Nat = {
    n.node match {
      case NatVar(index) => nameOf(index)
      case NatCst(value) => Cst(value)
      case NatAdd(a, b) => toNamedGeneric(a, nameOf) + toNamedGeneric(b, nameOf)
      case NatMul(a, b) => toNamedGeneric(a, nameOf) * toNamedGeneric(b, nameOf)
    }
  }

  /** Simplifies a nat by going through the arith expr library
    * @todo could use Vars instead of NamedVars for performance here */
  def simplify(n: Nat): Nat = {
    val r = fromNamedGeneric(
      toNamedGeneric(n, i => rct.NatIdentifier(s"n$i")),
      ni => ni.name.drop(1).toInt)
    r
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
  def fromNat(n: Nat): NatPattern = {
    val pnode = n.node.map(fromNat)
    NatPatternNode(pnode)
  }
}