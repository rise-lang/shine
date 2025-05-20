package rise.eqsat

import rise.core.{types => rct}
import rise.core
import rise.core.types.{DataType => rcdt}

/** A Rise arithmetic expression based on DeBruijn indexing */
case class Nat(node: NatNode[Nat]) {
  override def toString: String = node.toString
}

object Nat {
  import arithexpr.arithmetic._

  /** Shift nat indices */
  type Shift = (Int, Int)

  def fromNamed(n: rct.Nat, scope: Expr.Scope = Expr.Bound.empty): Nat = {
    Nat(n match {
      case i: rct.NatIdentifier => NatVar(scope.indexOf(i))
      case PosInf => NatPosInf
      case NegInf => NatNegInf
      case Cst(c) => NatCst(c)
      case Sum(Nil) => NatCst(0)
      case Sum(t +: ts) => ts.foldRight(fromNamed(t, scope)) { case (t, acc) =>
        Nat(NatAdd(fromNamed(t, scope), acc))
      }.node
      case Prod(Nil) => NatCst(1)
      case Prod(t +: ts) => ts.foldRight(fromNamed(t, scope)) { case (t, acc) =>
        Nat(NatMul(fromNamed(t, scope), acc))
      }.node
      case Pow(b, e) =>
        NatPow(fromNamed(b, scope), fromNamed(e, scope))
      case Mod(a, b) =>
        NatMod(fromNamed(a, scope), fromNamed(b, scope))
      case IntDiv(a, b) =>
        NatIntDiv(fromNamed(a, scope), fromNamed(b, scope))
      case _ => throw new Exception(s"no support for $n")
    })
  }

  def toNamed(n: Nat, scope: Expr.Scope = Expr.Bound.empty): rct.Nat = {
    n.node match {
      case NatVar(index) => scope.getNat(index)
      case NatCst(value) => Cst(value)
      case NatNegInf => NegInf
      case NatPosInf => PosInf
      case NatAdd(a, b) => toNamed(a, scope) + toNamed(b, scope)
      case NatMul(a, b) => toNamed(a, scope) * toNamed(b, scope)
      case NatPow(b, e) => toNamed(b, scope).pow(toNamed(e, scope))
      case NatMod(a, b) => toNamed(a, scope) % toNamed(b, scope)
      case NatIntDiv(a, b) => toNamed(a, scope) / toNamed(b, scope)
      case NatToNatApp(f, n) => rct.NatToNatApply(NatToNat.toNamed(f, scope), toNamed(n, scope))
    }
  }

  /** Simplifies a nat by going through the arith expr library
    * @todo could use Vars instead of NamedVars for performance here */
  def simplify(n: Nat): Nat = {
    case class SimplifyScope() extends Expr.Scope {
      def getExpr(i: Int): core.Identifier = ???
      def getNat(i: Int): rct.NatIdentifier = rct.NatIdentifier(s"n$i")
      def getData(i: Int): rcdt.DataTypeIdentifier = ???
      def getAddr(i: Int): rct.AddressSpaceIdentifier = ???
      def getN2N(i: Int): rct.NatToNatIdentifier = rct.NatToNatIdentifier(s"n2n$i")

      def indexOf(i: core.Identifier): Int = ???
      def indexOf(i: rct.NatIdentifier): Int = i.name.drop(1).toInt
      def indexOf(i: rcdt.DataTypeIdentifier): Int = ???
      def indexOf(i: rct.AddressSpaceIdentifier): Int = ???
      def indexOf(i: rct.NatToNatIdentifier): Int = i.name.drop(3).toInt

      def +(i: core.Identifier): Expr.Scope = ???
      def +(i: rct.NatIdentifier): Expr.Scope = this
      def +(i: rcdt.DataTypeIdentifier): Expr.Scope = ???
      def +(i: rct.AddressSpaceIdentifier): Expr.Scope = ???
      def +(i: rct.NatToNatIdentifier): Expr.Scope = this

      def bindExpr(t: rct.ExprType): (core.Identifier, Expr.Scope) = ???
      def bindNat(): (rct.NatIdentifier, Expr.Scope) = ??? // TODO
      def bindData(): (rcdt.DataTypeIdentifier, Expr.Scope) = ???
      def bindAddr(): (rct.AddressSpaceIdentifier, Expr.Scope) = ???
      def bindN2N(): (rct.NatToNatIdentifier, Expr.Scope) = ??? // TODO
    }
    val scope = SimplifyScope()
    val r = fromNamed(toNamed(n, scope), scope)
    r
  }
}

sealed trait NatNode[+N] {
  def map[O](f: N => O): NatNode[O] = this match {
    case v: NatVar => v
    case c: NatCst => c
    case NatPosInf => NatPosInf
    case NatNegInf => NatNegInf
    case NatAdd(a, b) => NatAdd(f(a), f(b))
    case NatMul(a, b) => NatMul(f(a), f(b))
    case NatPow(b, e) => NatPow(f(b), f(e))
    case NatMod(a, b) => NatMod(f(a), f(b))
    case NatIntDiv(a, b) => NatIntDiv(f(a), f(b))
    case NatToNatApp(n2n, n) => NatToNatApp(n2n.map(f), f(n))
  }
  def nats(): Iterator[N] = this match {
    case _: NatVar | _: NatCst | NatPosInf | NatNegInf => Iterator()
    case NatAdd(a, b) => Iterator(a, b)
    case NatMul(a, b) => Iterator(a, b)
    case NatPow(b, e) => Iterator(b, e)
    case NatMod(a, b) => Iterator(a, b)
    case NatIntDiv(a, b) => Iterator(a, b)
    case NatToNatApp(n2n, n) => n2n.nats() ++ Iterator(n)
  }
  def natCount(): Int = nats().size
}
case class NatVar(index: Int) extends NatNode[Nothing] {
  override def toString: String = s"%n$index"
}
case class NatCst(value: Long) extends NatNode[Nothing] {
  override def toString: String = value.toString
}
case object NatPosInf extends NatNode[Nothing] {
  override def toString: String = "+∞"
}
case object NatNegInf extends NatNode[Nothing] {
  override def toString: String = "-∞"
}
case class NatAdd[N](a: N, b: N) extends NatNode[N] {
  override def toString: String = s"($a + $b)"
}
case class NatMul[N](a: N, b: N) extends NatNode[N] {
  override def toString: String = s"($a * $b)"
}
case class NatPow[N](b: N, e: N) extends NatNode[N] {
  override def toString: String = s"($b ^ $e)"
}
case class NatMod[N](a: N, b: N) extends NatNode[N] {
  override def toString: String = s"($a % $b)"
}
case class NatIntDiv[N](a: N, b: N) extends NatNode[N] {
  override def toString: String = s"($a / $b)"
}
case class NatToNatApp[N](f: NatToNatNode[N], n: N) extends NatNode[N] {
  // override def toString: String = s"($f $n)"
}

sealed trait NatPattern {
  def patternVars(): Set[Any] = {
    this match {
      case NatPatternNode(n) => n.nats().flatMap(_.patternVars()).toSet
      case pv: NatPatternVar => Set(pv)
      case NatPatternAny => Set()
    }
  }
}
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