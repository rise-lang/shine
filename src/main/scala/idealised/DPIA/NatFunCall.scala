package idealised.DPIA

import lift.arithmetic.{ArithExpr, ArithExprFunction, Var}

sealed trait NatFunArg
case class NatArg(n:Nat) extends NatFunArg
case class LetNatIdArg(letNatIdentifier: LetNatIdentifier) extends NatFunArg


class NatFunCall(val fun:LetNatIdentifier, val args:Seq[NatFunArg]) extends ArithExprFunction(fun.id.name)  {
  override def visitAndRebuild(f: Nat => Nat): Nat = NatFunCall(fun, args.map {
    case NatArg(n) => NatArg(f(n))
    case other => other
  })

  override def freeVariables: Set[Var] = args.map({
    case NatArg(arg) => ArithExpr.freeVariables(arg)
    case _ => Set[Var]()
  }).reduceOption(_.union(_)).getOrElse(Set())

  def callAndParameterListString =
    s"${fun.id.name}(${args.map{
      arg =>
        val nat:Nat = arg match {
          case NatArg(n) => n
          case LetNatIdArg(LetNatIdentifier(id)) => id
        }
        nat.toString
    }.reduceOption(_ + "," + _).getOrElse("")})"

  override lazy val toString = s"⌈${this.callAndParameterListString}⌉"

  override val HashSeed = 0x31111112

  override def equals(that: Any): Boolean = that match {
    case f: NatFunCall => this.name.equals(f.name) && this.args == f.args
    case _ => false
  }
}

object NatFunCall {
  def apply(fun:LetNatIdentifier, args:Seq[NatFunArg]) = new NatFunCall(fun, args)

  def unapply(arg: NatFunCall): Option[(LetNatIdentifier, Seq[NatFunArg])] = Some((arg.fun, arg.args))
}
