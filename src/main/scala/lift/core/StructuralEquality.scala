package lift.core

import lift.core.types._
import lift.arithmetic.ArithExpr

object StructuralEquality {
  final case class Environment(idents: Map[String, String],
                               natIdents: Map[Nat, Nat],
                               typeIdents: Map[DataTypeIdentifier, DataTypeIdentifier]){
    def bindIdents(ab: (String, String)): Environment = {
      this.copy(idents = idents + ab)
    }

    def bindNatIdents(ab: (NatIdentifier, NatIdentifier)): Environment = {
      this.copy(natIdents = natIdents + ab)
    }

    def bindTypeIdents(ab: (DataTypeIdentifier, DataTypeIdentifier)): Environment = {
      this.copy(typeIdents = typeIdents + ab)
    }
  }

  def apply(a: Expr, b: Expr): Boolean = {
    apply(a, b, Environment(Map(), Map(), Map()))
  }

  def apply(a: Expr, b: Expr, env: Environment): Boolean = {
    def exp(a: Expr, b: Expr): Boolean =
      apply(a, b, env)
    def nat(a: Nat, b: Nat): Boolean =
      ArithExpr.substitute(a, env.natIdents) == b
    def typ(a: Type, b: Type): Boolean =
      true // TODO: types
    def data(a: DataType, b: DataType): Boolean =
      true // TODO: data types

    (a, b) match {
      case (Identifier(na), Identifier(nb)) =>
        env.idents.get(na) match {
          case Some(n) => n == nb
          case None => throw new Exception(s"unexpected free variable '$na'")
        }
      case (Lambda(xa, ba), Lambda(xb, bb)) =>
        apply(ba, bb, env bindIdents (xa.name, xb.name))
      case (Apply(fa, ea), Apply(fb, eb)) =>
        exp(fa, fb) && exp(ea, eb)
      case (DepLambda(xa: NatIdentifier, ba), DepLambda(xb: NatIdentifier, bb)) =>
        apply(ba, bb, env bindNatIdents (xa, xb))
      case (DepApply(fa, na: Nat), DepApply(fb, nb: Nat)) =>
        exp(fa, fb) && nat(na, nb)
      case (DepLambda(xa: DataTypeIdentifier, ba), DepLambda(xb: DataTypeIdentifier, bb)) =>
        apply(ba, bb, env bindTypeIdents (xa, xb))
      case (DepApply(fa, ta: DataType), DepApply(fb, tb: DataType)) =>
        exp(fa, fb) && data(ta, tb)
      case (la: Literal, lb: Literal) => la == lb
      case (Index(na, sa), Index(nb, sb)) =>
        nat(na, nb) && nat(sa, sb)
      case (NatExpr(na), NatExpr(nb)) =>
        nat(na, nb)
      case (TypedExpr(ea, ta), TypedExpr(eb, tb)) =>
        exp(ea, eb) && typ(ta, tb)
      case (pa: Primitive, pb: Primitive) => pa == pb
      case _ => false
    }
  }
}
