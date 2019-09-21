package idealised.DPIA

import lift.arithmetic.Cst

case class LetNatIdentifier(id:NatIdentifier) {
  def name:String = id.name

  def apply(args:Any*):NatFunCall = {
    NatFunCall(this, args.map({
      case n:Nat => NatArg(n)
      case int:Integer => NatArg(Cst(int.longValue()))
      case id:LetNatIdentifier => LetNatIdArg(id)
      case other =>
        throw new Exception(s"Invalid parameter to NatFunCall ${this.name} $other (of type ${other.getClass.getName}, must either bet a Nat or a LetNatIdentifier")
    }))
  }
}

object LetNatIdentifier {
  def apply():LetNatIdentifier = {
    LetNatIdentifier(NatIdentifier(freshName("nFun")))
  }
}
