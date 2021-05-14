package rise.core.types

import arithexpr.arithmetic.BoolExpr
import parser.{AddrConstraintError, BoolConstraintError, ConstraintError, DepAppConstraintError, NatCollectionConstraintError, NatConstraintError, NatToDataConstraintError, TypeConstraintError}
import rise.core.types.Solution.{AddrCTE, BoolCTE, DepCTE, NatCTE, NatToDataCTE, TypeCTE}
import rise.core.{Expr, substitute, traversal}

object Solution {
  def NatCTE(cTE: ConstraintError) = NatConstraintError(cTE.sp)
  def NatToDataCTE(cTE: ConstraintError) = NatToDataConstraintError(cTE.sp)
  def NatCollCTE(cTE: ConstraintError) = NatCollectionConstraintError(cTE.sp)
  def TypeCTE(cTE: ConstraintError) = TypeConstraintError(cTE.sp)
  def AddrCTE(cTE: ConstraintError) = AddrConstraintError(cTE.sp)
  def BoolCTE(cTE: ConstraintError) = BoolConstraintError(cTE.sp)
  def DepCTE(cTE: ConstraintError) = DepAppConstraintError(cTE.sp)

  def apply(): Solution = Solution(Map(), Map(), Map(), Map(), Map(), Map())
  def subs(ta: Type, tb: Type): Solution = {
    Solution(Map(ta -> tb), Map(), Map(), Map(), Map(), Map())
  }

  def subs(ta: DataTypeIdentifier, tb: Type): Solution =
    Solution(Map(ta -> tb), Map(), Map(), Map(), Map(), Map())
  def subs(na: NatIdentifier, nb: Nat): Solution =
    Solution(Map(), Map(na -> nb), Map(), Map(), Map(), Map())
  def subs(aa: AddressSpaceIdentifier, ab: AddressSpace): Solution =
    Solution(Map(), Map(), Map(aa -> ab), Map(), Map(), Map())
  def subs(na: NatToDataIdentifier, nb: NatToData): Solution =
    Solution(Map(), Map(), Map(), Map(na -> nb), Map(), Map())
  def subs(na: NatToNatIdentifier, nb: NatToNat): Solution =
    Solution(Map(), Map(), Map(), Map(), Map(na -> nb), Map())
  def subs(na: NatCollectionIdentifier, nb: NatCollection): Solution =
    Solution(Map(), Map(), Map(), Map(), Map(), Map(na -> nb))
}

case class Solution(ts: Map[Type, Type],
                    ns: Map[NatIdentifier, Nat],
                    as: Map[AddressSpaceIdentifier, AddressSpace],
                    n2ds: Map[NatToDataIdentifier, NatToData],
                    n2ns: Map[NatToNatIdentifier, NatToNat],
                    natColls: Map[NatCollectionIdentifier, NatCollection]
                   ) {
  import traversal.{Continue, Result, Stop}

  case class Visitor(sol: Solution) extends traversal.Visitor {
    override def visitNat(ae: Nat): Result[Nat] = Stop(sol(ae))
    override def visitType[T <: Type](t: T): Result[T] =
      Stop(sol(t).asInstanceOf[T])
    override def visitAddressSpace(a: AddressSpace): Result[AddressSpace] =
      Stop(sol(a))
    override def visitN2D(n2d: NatToData): Result[NatToData] = Stop(sol(n2d))

    override def visitN2N(n2n: NatToNat): Result[NatToNat] = Stop(sol(n2n))
  }

  def apply(e: Expr): Expr = {
    traversal.DepthFirstLocalResult(e, Visitor(this))
  }

  def apply(t: Type): Type = {
    traversal.types.DepthFirstLocalResult(t, new Visitor(this) {
      override def visitType[T <: Type](t: T): Result[T] =
        sol.ts.get(t) match {
          case Some(x) => Stop(x.asInstanceOf[T])
          case None    => Continue(t, this)
        }
    })
  }

  def apply(n: Nat): Nat =
    (substitute.natsInNat(ns, _)).andThen(substitute.n2nsInNat(n2ns, _))(n)

  def apply(b: BoolExpr): BoolExpr = {
    import arithexpr.arithmetic.ArithExpr
    b.substitute(ns.asInstanceOf[Map[ArithExpr, ArithExpr]]).getOrElse(b)
  }

  def apply(a: AddressSpace): AddressSpace =
    substitute.addressSpacesInAddressSpace(as, a)

  def apply(a: NatCollection): NatCollection =
    substitute.natCollectionInNatCollection(natColls, a)

  def apply(n2d: NatToData): NatToData = {
    substitute.n2dsInN2d(n2ds, n2d) match {
      case id: NatToDataIdentifier => id
      case lambda@NatToDataLambda(x, body) =>
        val xSub = apply(x) match {
          case n: NatIdentifier => n
          case n: arithexpr.arithmetic.NamedVar => NatIdentifier(n.name, n.range, isExplicit = true)
          case other => throw NonIdentifierInBinderException(lambda, other)
        }
        NatToDataLambda(xSub, apply(body).asInstanceOf[DataType])
    }
  }

  def apply(n2n: NatToNat): NatToNat = {
    substitute.n2nsInN2n(n2ns, n2n) match {
      case id: NatToNatIdentifier => id
      case lambda@NatToNatLambda(x, body) =>
        val xSub = apply(x) match {
          case n: NatIdentifier => n
          case other => throw NonIdentifierInBinderException(lambda, other)
        }
        NatToNatLambda(xSub, apply(body))
    }
  }

  // concatenating two solutions into a single one
  def ++(other: Solution): Solution = {
    // this function combines two solutions by applying all the solutions
    // from s2 to the values in s1 it then concatenates the resulting maps
    val combine: (Solution, Solution) => Solution = (s1, s2) => {
      Solution(
        (s1.ts.view.mapValues(t => s2(t)) ++ s2.ts).toMap,
        (s1.ns.view.mapValues(n => s2(n)) ++ s2.ns).toMap,
        (s1.as.view.mapValues(a => s2(a)) ++ s2.as).toMap,
        (s1.n2ds.view.mapValues(n => s2(n)) ++ s2.n2ds).toMap,
        (s1.n2ns.view.mapValues(n => s2(n)) ++ s2.n2ns).toMap,
        (s1.natColls.view.mapValues(n => s2(n)) ++ s2.natColls).toMap
      )
    }

    // concatenating two solutions starts by combining them ...
    combine(this, other)
  }

  def apply(constraints: Seq[Constraint]): Seq[Constraint] = {
    constraints.map {
      case TypeConstraint(a, b, cTE) =>
        TypeConstraint(apply(a), apply(b), TypeCTE(cTE))
      case AddressSpaceConstraint(a, b, cTE) =>
        AddressSpaceConstraint(apply(a), apply(b), AddrCTE(cTE))
      case NatConstraint(a, b, cTE) =>
        NatConstraint(apply(a), apply(b), NatCTE(cTE))
      case BoolConstraint(a, b, cTE) =>
        BoolConstraint(apply(a), apply(b), BoolCTE(cTE))
      case NatToDataConstraint(a, b, cTE) =>
        NatToDataConstraint(apply(a), apply(b), NatToDataCTE(cTE))
      case DepConstraint(df, arg: Nat, t, cTE) =>
        DepConstraint[NatKind](apply(df), apply(arg), apply(t), DepCTE(cTE))
      case DepConstraint(df, arg: DataType, t, cTE) =>
        DepConstraint[DataKind](
          apply(df),
          apply(arg).asInstanceOf[DataType],
          apply(t), DepCTE(cTE)
        )
      case DepConstraint(df, arg: AddressSpace, t, cTE) =>
        DepConstraint[AddressSpaceKind](apply(df), apply(arg), apply(t), DepCTE(cTE))
    }
  }
}
