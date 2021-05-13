package rise.core.types

import util.monads._
import arithexpr.arithmetic.{BoolExpr, NamedVar}
import rise.core.traverse._
import rise.core.{Expr, substitute}

object Solution {
  def apply(): Solution = Solution(Map(), Map(), Map(), Map(), Map(), Map(), Map(), Map())
  def subs(ta: Type, tb: Type): Solution = {
    Solution(Map(ta -> tb), Map(), Map(), Map(), Map(), Map(), Map(), Map())
  }

  def subs(ta: DataTypeIdentifier, tb: Type): Solution =
    Solution(Map(ta -> tb), Map(), Map(), Map(), Map(), Map(), Map(), Map())
  def subs(na: NatIdentifier, nb: Nat): Solution =
    Solution(Map(), Map(na -> nb), Map(), Map(), Map(), Map(), Map(), Map())
  def subs(aa: AddressSpaceIdentifier, ab: AddressSpace): Solution =
    Solution(Map(), Map(), Map(aa -> ab), Map(), Map(), Map(), Map(), Map())
  def subs(ma: MatrixLayoutIdentifier, mb: MatrixLayout): Solution =
    Solution(Map(), Map(), Map(), Map(ma -> mb), Map(), Map(),  Map(), Map())
  def subs(fa: FragmentKindIdentifier, fb: FragmentKind): Solution =
    Solution(Map(), Map(), Map(), Map(), Map(fa -> fb), Map(),  Map(), Map())
  def subs(na: NatToDataIdentifier, nb: NatToData): Solution =
    Solution(Map(), Map(), Map(), Map(), Map(), Map(na -> nb), Map(), Map())
  def subs(na: NatToNatIdentifier, nb: NatToNat): Solution =
    Solution(Map(), Map(), Map(), Map(), Map(), Map(), Map(na -> nb), Map())
  def subs(na: NatCollectionIdentifier, nb: NatCollection): Solution =
    Solution(Map(), Map(), Map(), Map(), Map(), Map(), Map(), Map(na -> nb))
}

case class Solution(ts: Map[Type, Type],
                    ns: Map[NatIdentifier, Nat],
                    as: Map[AddressSpaceIdentifier, AddressSpace],
                    ms: Map[MatrixLayoutIdentifier, MatrixLayout],
                    fs: Map[FragmentKindIdentifier, FragmentKind],
                    n2ds: Map[NatToDataIdentifier, NatToData],
                    n2ns: Map[NatToNatIdentifier, NatToNat],
                    natColls: Map[NatCollectionIdentifier, NatCollection]
                   ) {

  abstract class Visitor(sol: Solution) extends PureTraversal {
    override def nat: Nat => Pure[Nat] = ae => return_(sol(ae))
    override def addressSpace: AddressSpace => Pure[AddressSpace] = a => return_(sol(a))
    override def matrixLayout: MatrixLayout => Pure[MatrixLayout] = m => return_(sol(m))
    override def fragmentKind: FragmentKind => Pure[FragmentKind] = f => return_(sol(f))
    override def natToData: NatToData => Pure[NatToData] = n2d => return_(sol(n2d))
    override def natToNat: NatToNat => Pure[NatToNat] = n2n => return_(sol(n2n))
  }

  case class V(sol: Solution) extends Visitor(sol) {
    override def `type`[T <: Type] : T => Pure[T] = t => return_(sol(t).asInstanceOf[T])
  }
  def apply(e: Expr): Expr = V(this).expr(e).unwrap

  case class T(sol: Solution) extends Visitor(sol) {
    override def datatype: DataType => Pure[DataType] = {
      case t: DataTypeIdentifier => return_(sol.ts.getOrElse(t, t).asInstanceOf[DataType])
      case t => super.datatype(t)
    }
    override def `type`[T <: Type]: T => Pure[T] = {
      case t : TypeIdentifier => return_(sol.ts.getOrElse(t, t).asInstanceOf[T])
      case t => super.`type`(t)
    }
  }
  def apply(t: Type): Type = T(this).`type`(t).unwrap

  def apply(n: Nat): Nat =
    (substitute.natsInNat(ns, _)).andThen(substitute.n2nsInNat(n2ns, _))(n)

  def apply(b: BoolExpr): BoolExpr = {
    import arithexpr.arithmetic.ArithExpr
    b.substitute(ns.asInstanceOf[Map[ArithExpr, ArithExpr]]).getOrElse(b)
  }

  def apply(a: AddressSpace): AddressSpace =
    substitute.addressSpacesInAddressSpace(as, a)

  def apply(m: MatrixLayout): MatrixLayout =
    substitute.matrixLayoutsInMatrixLayout(ms, m)

  def apply(f: FragmentKind): FragmentKind =
    substitute.fragmentTypesInFragmentType(fs, f)

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
          case n: arithexpr.arithmetic.NamedVar => NatIdentifier(n.name, n.range, isExplicit = true)
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
        (s1.ms.view.mapValues(m => s2(m)) ++ s2.ms).toMap,
        (s1.fs.view.mapValues(f => s2(f)) ++ s2.fs).toMap,
        (s1.n2ds.view.mapValues(n => s2(n)) ++ s2.n2ds).toMap,
        (s1.n2ns.view.mapValues(n => s2(n)) ++ s2.n2ns).toMap,
        (s1.natColls.view.mapValues(n => s2(n)) ++ s2.natColls).toMap
      )
    }

    // concatenating two solutions starts by combining them ...
    combine(this, other)
  }

  // def apply(constraints: Seq[Constraint]): Seq[Constraint] = constraints.map(apply)
  def apply(constraints: Seq[ScopedConstraint]): Seq[ScopedConstraint] =
    constraints.map {case ScopedConstraint(c, s) => ScopedConstraint(apply(c), s)}

  def apply(constraint: Constraint): Constraint = constraint match {
    case TypeConstraint(a, b) => TypeConstraint(apply(a), apply(b))
    case NatConstraint(a, b) => NatConstraint(apply(a), apply(b))
    case BoolConstraint(a, b) => BoolConstraint(apply(a), apply(b))
    case MatrixLayoutConstraint(a, b) => MatrixLayoutConstraint(apply(a), apply(b))
    case FragmentTypeConstraint(a, b) => FragmentTypeConstraint(apply(a), apply(b))
    case NatToDataConstraint(a, b) => NatToDataConstraint(apply(a), apply(b))
    case NatCollectionConstraint(a, b) => NatCollectionConstraint(apply(a), apply(b))
    case DepConstraint(df, arg: Nat, t) => DepConstraint[NatKind](apply(df), apply(arg), apply(t))
    case DepConstraint(df, arg: DataType, t) =>
      DepConstraint[DataKind](apply(df), apply(arg).asInstanceOf[DataType], apply(t))
    case DepConstraint(df, arg: Type, t) =>
      DepConstraint[TypeKind](apply(df), apply(arg), apply(t))
    case DepConstraint(df, arg: AddressSpace, t) =>
      DepConstraint[AddressSpaceKind](apply(df), apply(arg), apply(t))
    case DepConstraint(df, arg: NatToData, t) =>
      DepConstraint[NatToDataKind](apply(df), apply(arg), apply(t))
    case DepConstraint(df, arg: NatToNat, t) =>
      DepConstraint[NatToNatKind](apply(df), apply(arg), apply(t))
    case DepConstraint(df, arg: NatCollection, t) =>
      DepConstraint[NatCollectionKind](apply(df), apply(arg), apply(t))
    case DepConstraint(_, _, _) => throw new Exception("Impossible case")
  }
}
