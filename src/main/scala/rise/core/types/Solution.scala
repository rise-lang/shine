package rise.core.types

import rise.core.{Expr, substitute, traversal}

object Solution {
  def apply(): Solution = Solution(Map(), Map(), Map(), Map())
  def subs(ta: Type, tb: Type): Solution = {
    Solution(Map(ta -> tb), Map(), Map(), Map())
  }

  def subs(ta: DataTypeIdentifier, tb: Type): Solution =
    Solution(Map(ta -> tb), Map(), Map(), Map())
  def subs(na: NatIdentifier, nb: Nat): Solution =
    Solution(Map(), Map(na -> nb), Map(), Map())
  def subs(aa: AddressSpaceIdentifier, ab: AddressSpace): Solution =
    Solution(Map(), Map(), Map(aa -> ab), Map())
  def subs(na: NatToDataIdentifier, nb: NatToData): Solution =
    Solution(Map(), Map(), Map(), Map(na -> nb))
}

case class Solution(ts: Map[Type, Type],
                    ns: Map[NatIdentifier, Nat],
                    as: Map[AddressSpaceIdentifier, AddressSpace],
                    n2ds: Map[NatToDataIdentifier, NatToData]
                   ) {
  import traversal.{Continue, Result, Stop}

  case class Visitor(sol: Solution) extends traversal.Visitor {
    override def visitNat(ae: Nat): Result[Nat] = Stop(sol(ae))
    override def visitType[T <: Type](t: T): Result[T] =
      Stop(sol(t).asInstanceOf[T])
    override def visitAddressSpace(a: AddressSpace): Result[AddressSpace] =
      Stop(sol(a))
    override def visitN2D(n2d: NatToData): Result[NatToData] = Stop(sol(n2d))
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
    substitute.natsInNat(ns, n)

  def apply(a: AddressSpace): AddressSpace =
    substitute.addressSpacesInAddressSpace(as, a)

  def apply(n2d: NatToData): NatToData = {
    substitute.n2dsInN2d(n2ds, n2d) match {
      case id: NatToDataIdentifier => id
      case lambda@NatToDataLambda(x, body) =>
        val xSub = apply(x) match {
          case n: NatIdentifier => n
          case other => throw NonIdentifierInBinderException(lambda, other)
        }
        NatToDataLambda(xSub, apply(body).asInstanceOf[DataType])
    }
  }

  // concatenating two solutions into a single one
  def ++(other: Solution): Solution = {
    // this function combines two solutions by applying all the solutions
    // from s2 to the values in s1 it then concatenates the resulting maps
    val combine: (Solution, Solution) => Solution = (s1, s2) => {
      Solution(
        s1.ts.mapValues(t => s2(t)) ++ s2.ts,
        s1.ns.mapValues(n => s2(n)) ++ s2.ns,
        s1.as.mapValues(a => s2(a)) ++ s2.as,
        s1.n2ds.mapValues(n => s2(n)) ++ s2.n2ds
      )
    }

    // concatenating two solutions starts by combining them ...
    combine(this, other)
  }

  def apply(constraints: Seq[Constraint]): Seq[Constraint] = {
    constraints.map {
      case TypeConstraint(a, b) =>
        TypeConstraint(apply(a), apply(b))
      case AddressSpaceConstraint(a, b) =>
        AddressSpaceConstraint(apply(a), apply(b))
      case NatConstraint(a, b) =>
        NatConstraint(apply(a), apply(b))
      case NatToDataConstraint(a, b) =>
        NatToDataConstraint(apply(a), apply(b))
      case DepConstraint(df, arg: Nat, t) =>
        DepConstraint[NatKind](apply(df), apply(arg), apply(t))
      case DepConstraint(df, arg: DataType, t) =>
        DepConstraint[DataKind](
          apply(df),
          apply(arg).asInstanceOf[DataType],
          apply(t)
        )
      case DepConstraint(df, arg: AddressSpace, t) =>
        DepConstraint[AddressSpaceKind](apply(df), apply(arg), apply(t))
    }
  }
}
