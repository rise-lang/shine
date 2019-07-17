package lift.core

import lift.core.types._
import lift.arithmetic.ArithExpr

object StructuralEquality {
  final case class Environment(expIdents: Map[String, String],
                               natIdents: Map[Nat, Nat],
                               typIdents: Map[DataTypeIdentifier, DataTypeIdentifier]){
    def bindExpIdents(ab: (String, String)): Environment = {
      this.copy(expIdents = expIdents + ab)
    }

    def bindNatIdents(ab: (NatIdentifier, NatIdentifier)): Environment = {
      this.copy(natIdents = natIdents + ab)
    }

    def bindTypIdents(ab: (DataTypeIdentifier, DataTypeIdentifier)): Environment = {
      this.copy(typIdents = typIdents + ab)
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
      apply(a, b, env)

    (a, b) match {
      case (Identifier(na), Identifier(nb)) =>
        env.expIdents.get(na) match {
          case Some(n) => n == nb
          case None => throw new Exception(s"unexpected free variable '$na'")
        }
      case (Lambda(xa, ba), Lambda(xb, bb)) =>
        apply(ba, bb, env bindExpIdents (xa.name, xb.name))
      case (Apply(fa, ea), Apply(fb, eb)) =>
        exp(fa, fb) && exp(ea, eb)
      case (DepLambda(xa: NatIdentifier, ba), DepLambda(xb: NatIdentifier, bb)) =>
        apply(ba, bb, env bindNatIdents (xa, xb))
      case (DepApply(fa, na: Nat), DepApply(fb, nb: Nat)) =>
        exp(fa, fb) && nat(na, nb)
      case (DepLambda(xa: DataTypeIdentifier, ba), DepLambda(xb: DataTypeIdentifier, bb)) =>
        apply(ba, bb, env bindTypIdents (xa, xb))
      case (DepApply(fa, ta: DataType), DepApply(fb, tb: DataType)) =>
        exp(fa, fb) && typ(ta, tb)
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

  def apply(a: Type, b: Type): Boolean = {
    apply(a, b, Environment(Map(), Map(), Map()))
  }

  def apply(a: Type, b: Type, env: Environment): Boolean = {
    def nat(a: Nat, b: Nat): Boolean =
      ArithExpr.substitute(a, env.natIdents) == b
    def typ(a: Type, b: Type): Boolean =
      apply(a, b, env)
    
    (a, b) match {
      case (dta: DataTypeIdentifier, dtb: DataTypeIdentifier) =>
        env.typIdents.get(dta) match {
          case Some(dt) => dt == dtb
          case None => throw new Exception(s"unexpected free variable '$dta'")
        }
      case (ArrayType(na, ea), ArrayType(nb, eb)) =>
        nat(na, nb) && typ(ea, eb)
      case (TupleType(tas@_*), TupleType(tbs@_*)) =>
        tas.zip(tbs).foldLeft(true){ case (eq, (ta, tb)) => eq && typ(ta, tb) }
      case (sa: ScalarType, sb: ScalarType) =>
       sa == sb
      case (IndexType(na), IndexType(nb)) =>
        nat(na, nb)
      case (VectorType(na, ea), VectorType(nb, eb)) =>
        nat(na, nb) && typ(ea, eb)
      case (FunctionType(ia, oa), FunctionType(ib, ob)) =>
        typ(ia, ib) && typ(oa, ob)
      case (DependentFunctionType(na: NatIdentifier, ta), DependentFunctionType(nb: NatIdentifier, tb)) =>
        apply(ta, tb, env bindNatIdents(na, nb))
      case (DependentFunctionType(dta: DataTypeIdentifier, ta), DependentFunctionType(dtb: DataTypeIdentifier, tb)) =>
        apply(ta, tb, env bindTypIdents(dta, dtb))
      case _ => false
    }
  }
}
