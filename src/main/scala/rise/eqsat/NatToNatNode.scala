package rise.eqsat

import rise.core.{types => rct}

sealed trait NatToNatNode[+N] {
  def map[O](f: N => O): NatToNatNode[O] = this match {
    case n2n: NatToNatVar => n2n
    case NatToNatLambda(e) => NatToNatLambda(f(e))
  }

  def nats(): Iterator[N] = this match {
    case n2n: NatToNatVar => Iterator()
    case NatToNatLambda(e) => Iterator(e)
  }
}

case class NatToNatVar(index: Int) extends NatToNatNode[Nothing] {
    override def toString: String = s"n2n$index"
}
case class NatToNatLambda[N](e: N) extends NatToNatNode[N]

object NatToNat {
  type Shift = Nat.Shift
  
  def fromNamed(n2n: rct.NatToNat, scope: Expr.Scope = Expr.Bound.empty): NatToNat ={
    n2n match {
        case i: rct.NatToNatIdentifier => NatToNatVar(scope.indexOf(i))
        case rct.NatToNatLambda(x, body) => NatToNatLambda(Nat.fromNamed(body, scope + x))
    }
  }

  def toNamed(n2n: NatToNat, scope: Expr.Scope = Expr.Bound.empty): rct.NatToNat = {
    n2n match {
        case NatToNatVar(index) => scope.getN2N(index)
        case NatToNatLambda(e) =>
          val (i, scope2) = scope.bindNat()
          rct.NatToNatLambda(i, Nat.toNamed(e, scope2))
    }
  }
}