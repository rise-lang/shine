package rise.core.types

import arithexpr.arithmetic._
import rise.core.types


object NatIdentifier {
  def apply(name: String): NatIdentifier = new NamedVar(name)
  def apply(name: String, range: Range): NatIdentifier = new NamedVar(name, range)
}

final class NatToNatApply(val f: NatToNat, val n: Nat)
  extends ArithExprFunctionCall(s"$f($n)") {
  override def visitAndRebuild(fun: Nat => Nat): Nat =
    fun(NatToNatApply(this.f, fun(n)))

  override def substitute(subs: collection.Map[ArithExpr, ArithExpr]
                         ): Option[ArithExpr] =
    // TODO? subs in 'f'
    n.substitute(subs).map(NatToNatApply(f, _))

  override lazy val toString: String = s"$f($n)"

  override def freeVariables: Set[Var] =
    // TODO? ArithExpr.freeVariables(f)
    ArithExpr.freeVariables(n)

  override def exposedArgs: Seq[Nat] = Seq(n)

  override def substituteExposedArgs(
                                      subMap: Map[Nat, SimplifiedExpr]
                                    ): ArithExprFunctionCall =
    new NatToNatApply(f, subMap.getOrElse(n, n))
}

object NatToNatApply {
  def apply(f: NatToNat, n: Nat): Nat = f match {
    case l: NatToNatLambda     => l.apply(n)
    case i: NatToNatIdentifier => new NatToNatApply(i, n)
  }
  def unapply(arg: NatToNatApply): Option[(NatToNat, Nat)] =
    Some((arg.f, arg.n))
}
