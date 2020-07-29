package rise.core.types

import rise.core.TypeLevelDSL.n2dtFun
import rise.core.{freshName, substitute}
import rise.core.lifting.liftDependentFunctionType
import rise.core.types.InferenceException.error

import scala.collection.mutable

trait Constraint
case class TypeConstraint(a: Type, b: Type) extends Constraint {
  override def toString: String = s"$a  ~  $b"
}
case class NatConstraint(a: Nat, b: Nat) extends Constraint {
  override def toString: String = s"$a  ~  $b"
}
case class AddressSpaceConstraint(a: AddressSpace, b: AddressSpace)
  extends Constraint {
  override def toString: String = s"$a  ~  $b"
}
case class NatToDataConstraint(a: NatToData, b: NatToData)
  extends Constraint {
  override def toString: String = s"$a  ~  $b"
}
case class DepConstraint[K <: Kind](df: Type, arg: K#T, t: Type)
  extends Constraint {
  override def toString: String = s"$df ($arg) ~ $t"
}
case class NatCollectionConstraint(a: NatCollection, b: NatCollection)
  extends Constraint {
  override def toString: String = s"$a ~ $b"
}

object Constraint {
  def solve(cs: Seq[Constraint], trace: Seq[Constraint]): Solution = cs match {
    case Nil => Solution()
    case c +: cs =>
      val s = solveOne(c, trace)
      s ++ solve(s.apply(cs), trace)
  }

  // scalastyle:off method.length
  def solveOne(c: Constraint, trace: Seq[Constraint]): Solution = {
    implicit val _trace: Seq[Constraint] = trace
    def decomposed(cs: Seq[Constraint]) = solve(cs, c +: trace)

    c match {
      case TypeConstraint(a, b) =>
        (a, b) match {
          case (TypePlaceholder, _) => Solution()
          case (_, TypePlaceholder) => Solution()
          case (i: TypeIdentifier, _) => unifyTypeIdent(i, b)
          case (_, i: TypeIdentifier) => unifyTypeIdent(i, a)
          case (i: DataTypeIdentifier, dt: DataType) =>
            unifyDataTypeIdent(i, dt)
          case (dt: DataType, i: DataTypeIdentifier) =>
            unifyDataTypeIdent(i, dt)
          case (_: ScalarType | _: NatType.type |
                _: IndexType  | _: VectorType,
          _: ScalarType | _: NatType.type |
          _: IndexType  | _: VectorType)
            if a == b => Solution()
          case (IndexType(sa), IndexType(sb)) =>
            decomposed(Seq(NatConstraint(sa, sb)))
          case (ArrayType(sa, ea), ArrayType(sb, eb)) =>
            decomposed(Seq(NatConstraint(sa, sb), TypeConstraint(ea, eb)))
          case (VectorType(sa, ea), VectorType(sb, eb)) =>
            decomposed(Seq(NatConstraint(sa, sb), TypeConstraint(ea, eb)))
          case (DepArrayType(sa, ea), DepArrayType(sb, eb)) =>
            decomposed(Seq(NatConstraint(sa, sb), NatToDataConstraint(ea, eb)))
          case (PairType(pa1, pa2), PairType(pb1, pb2)) =>
            decomposed(Seq(TypeConstraint(pa1, pb1), TypeConstraint(pa2, pb2)))
          case (FunType(ina, outa), FunType(inb, outb)) =>
            decomposed(
              Seq(TypeConstraint(ina, inb), TypeConstraint(outa, outb))
            )
          case (
            DepFunType(na: NatIdentifier, ta),
            DepFunType(nb: NatIdentifier, tb)
            ) =>
            val n = NatIdentifier(freshName("n"), isExplicit = true)
            decomposed(
              Seq(
                NatConstraint(n, na.asImplicit),
                NatConstraint(n, nb.asImplicit),
                TypeConstraint(ta, tb)
              )
            )
          case (
            DepFunType(dta: DataTypeIdentifier, ta),
            DepFunType(dtb: DataTypeIdentifier, tb)
            ) =>
            val dt = DataTypeIdentifier(freshName("t"), isExplicit = true)
            decomposed(
              Seq(
                TypeConstraint(dt, dta.asImplicit),
                TypeConstraint(dt, dtb.asImplicit),
                TypeConstraint(ta, tb)
              )
            )
          case (
            DepFunType(_: AddressSpaceIdentifier, _),
            DepFunType(_: AddressSpaceIdentifier, _)
            ) =>
            ???

          case (
            DepPairType(x1: NatIdentifier, t1),
            DepPairType(x2: NatIdentifier, t2)
            ) =>
            val n = NatIdentifier(freshName("n"), isExplicit = true)

            decomposed(Seq(
              NatConstraint(n, x1.asImplicit),
              NatConstraint(n, x2.asImplicit),
              TypeConstraint(t1, t2)
            ))

          case (
            DepPairType(x1: NatCollectionIdentifier, t1),
            DepPairType(x2: NatCollectionIdentifier, t2)
            ) =>
            val n = NatCollectionIdentifier(freshName("n"), isExplicit = true)

            decomposed(Seq(
              NatCollectionConstraint(n, x1.asImplicit),
              NatCollectionConstraint(n, x2.asImplicit),
              TypeConstraint(t1, t2)
            ))

          case (
            NatToDataApply(f: NatToDataIdentifier, _),
            NatToDataApply(g, _)
            ) => Solution.subs(f, g)
          case (
            NatToDataApply(g, _),
            NatToDataApply(f: NatToDataIdentifier, _)
            ) => Solution.subs(f, g)
          case (
            NatToDataApply(f: NatToDataIdentifier, n: NatIdentifier),
            dt: DataType
            ) =>
            Solution.subs(f, n2dtFun(
              fresh => substitute.natInDataType(fresh, n, dt)
            ))
          case (
            dt: DataType,
            NatToDataApply(f: NatToDataIdentifier, n: NatIdentifier)
            ) =>
            Solution.subs(f, n2dtFun(
              fresh => substitute.natInDataType(fresh, n, dt)
            ))
          case (_: NatToDataApply, dt: DataType) =>
            Solution.subs(a, dt) // substitute apply by data type
          case (dt: DataType, _: NatToDataApply) =>
            Solution.subs(b, dt) // substitute apply by data type

          case _ =>
            error(s"cannot unify $a and $b")
        }


      case DepConstraint(df, arg, t) =>
        df match {
          case _: DepFunType[_, _] =>
            decomposed(
              Seq(TypeConstraint(liftDependentFunctionType(df)(arg), t))
            )
          case _ =>
            error(s"expected a dependent function type, but got $df")
        }

      case NatConstraint(a, b) =>
        (a, b) match {
          case (i: NatIdentifier, _) => nat.unifyIdent(i, b)
          case (_, i: NatIdentifier) => nat.unifyIdent(i, a)
          case _ if a == b           => Solution()
          // case _ if !nat.potentialPivots(a).isEmpty => nat.tryPivots(a, b)
          // case _ if !nat.potentialPivots(b).isEmpty => nat.tryPivots(b, a)
          case (s: arithexpr.arithmetic.Sum, _)    => nat.unifySum(s, b)
          case (_, s: arithexpr.arithmetic.Sum)    => nat.unifySum(s, a)
          case (p: arithexpr.arithmetic.Prod, _)   => nat.unifyProd(p, b)
          case (_, p: arithexpr.arithmetic.Prod)   => nat.unifyProd(p, a)
          case (p: arithexpr.arithmetic.Pow, _)    => nat.unifyProd(p, b)
          case (_, p: arithexpr.arithmetic.Pow)    => nat.unifyProd(p, a)
          case (p: arithexpr.arithmetic.IntDiv, _) => nat.unifyProd(p, b)
          case (_, p: arithexpr.arithmetic.IntDiv) => nat.unifyProd(p, a)
          case _ => error(s"cannot unify $a and $b")
        }

      case NatToDataConstraint(a, b) =>
        (a, b) match {
          case (i: NatToDataIdentifier, _) => natToData.unifyIdent(i, b)
          case (_, i: NatToDataIdentifier) => natToData.unifyIdent(i, a)
          case _ if a == b                 => Solution()
          case (NatToDataLambda(x1, dt1), NatToDataLambda(x2, dt2)) =>
            val n = NatIdentifier(freshName("n"), isExplicit = true)
            decomposed(Seq(
              NatConstraint(n, x1.asImplicit),
              NatConstraint(n, x2.asImplicit),
              TypeConstraint(dt1, dt2)
            ))

          case _ => error(s"cannot unify $a and $b")
        }

      case NatCollectionConstraint(a, b) =>
        (a,b) match {
          case (i: NatCollectionIdentifier, _) => natCollection.unifyIdent(i, b)
          case (_, i: NatCollectionIdentifier) => natCollection.unifyIdent(i, b)
          case _ if a == b                    => Solution()
          case (NatCollectionFromArray(e1), NatCollectionFromArray(e2)) =>
            // What to do here???
            ???
        }

    }
  }
  // scalastyle:on method.length

  def unifyTypeIdent(i: TypeIdentifier, t: Type): Solution = {
    Solution.subs(i, t)
  }

  // FIXME: datatypes and types are mixed up
  def unifyDataTypeIdent(i: DataTypeIdentifier, t: DataType)(
    implicit trace: Seq[Constraint]
  ): Solution = {
    t match {
      case j: DataTypeIdentifier =>
        if (i == j) {
          Solution()
        } else if (!i.isExplicit) {
          Solution.subs(i, j)
        } else if (!j.isExplicit) {
          Solution.subs(j, i)
        } else {
          error(s"cannot unify $i and $j, they are both explicit")
        }
      case _ if i.isExplicit =>
        error(s"cannot substitute $i, it is explicit")
      case _ if occurs(i, t) => error(s"circular use: $i occurs in $t")
      case _ if !i.isExplicit => Solution.subs(i, t)
    }
  }

  private object nat {
    import arithexpr.arithmetic._

    def freeOccurences(n: Nat): Map[NatIdentifier, Integer] = {
      val free_occurrences = mutable
        .Map[NatIdentifier, Integer]()
        .withDefault(_ => 0)
      ArithExpr.visit(n, {
        case v: NatIdentifier if !v.isExplicit => free_occurrences(v) += 1
        case _                                 =>
      })
      free_occurrences.toMap
    }

    // collect free variables with only 1 occurrence
    def potentialPivots(n: Nat): Set[NatIdentifier] = {
      freeOccurences(n).foldLeft(Set[NatIdentifier]())({
        case (potential, (v, c)) =>
          if (c == 1) {
            potential + v
          } else {
            potential
          }
      })
    }

    @scala.annotation.tailrec
    def pivotSolution(
                       pivot: NatIdentifier,
                       n: Nat,
                       value: Nat
                     ): Option[Solution] = {
      n match {
        case i: NatIdentifier if i == pivot => Some(Solution.subs(pivot, value))
        case Prod(terms) =>
          val (p, rest) = terms.partition(t => ArithExpr.contains(t, pivot))
          if (p.size != 1) {
            None
          } else {
            pivotSolution(pivot, p.head, rest.foldLeft(value)({
              case (v, r) => v /^ r
            }))
          }
        case Sum(terms) =>
          val (p, rest) = terms.partition(t => ArithExpr.contains(t, pivot))
          if (p.size != 1) {
            None
          } else {
            pivotSolution(pivot, p.head, rest.foldLeft(value)({
              case (v, r) => v - r
            }))
          }
        case Pow(b, Cst(-1)) => pivotSolution(pivot, b, Cst(1) /^ value)
        case _               => None
      }
    }

    def tryPivots(n: Nat, value: Nat)(
      implicit trace: Seq[Constraint]
    ): Solution = {
      potentialPivots(n).foreach(pivotSolution(_, n, value) match {
        case Some(s) => return s
        case None    =>
      })
      error(s"could not pivot $n = $value")
    }

    def unifyProd(p: Nat, n: Nat)(implicit trace: Seq[Constraint]): Solution = {
      // n = p --> 1 = p * (1/n)
      tryPivots(p /^ n, 1)
    }

    def unifySum(s: Sum, n: Nat)(implicit trace: Seq[Constraint]): Solution = {
      // n = s --> 0 = s + (-n)
      /** TODO(federico) I have no idea how this pivot system works for
       *  unifying natural numbers. I need to solve a case like
       *  x + 1 = y + 1. The bruteForceSum function does exactly that:
       *  see if the pattern matches for some constant, and then unify
       *  x and y.
       *
       *  Upon understanding the pivoting system, this should be cleaned up.
       */
      (n match {
        case s2: Sum => bruteForceSum(s, s2)
        case _ => None
      }).getOrElse(tryPivots(s - n, 0))
    }

    private def bruteForceSum(s1: Sum, s2: Sum): Option[Solution] = {
      val sortedS1 = s1.terms.sortBy({
        case Cst(_) => 0
        case _ => 1
      })
      val sortedS2 = s2.terms.sortBy({
        case Cst(_) => 0
        case _ => 1
      })

      (sortedS1, sortedS2) match {
        case ((c1: Cst)::(x: NatIdentifier)::Nil, c2::e::Nil) if c1 == c2 =>
          Some(Solution.subs(x,e))
        case ((c1: Cst)::e::Nil, c2::(x: NatIdentifier)::Nil) if c1 == c2 =>
          Some(Solution.subs(x, e))
        case _ => None
      }
    }

    def unifyIdent(i: NatIdentifier, n: Nat)(
      implicit trace: Seq[Constraint]
    ): Solution = n match {
      case j: NatIdentifier =>
        if (i == j) {
          Solution()
        } else if (!i.isExplicit) {
          Solution.subs(i, j)
        } else if (!j.isExplicit) {
          Solution.subs(j, i)
        } else {
          error(s"cannot unify $i and $j, they are both bound")
        }
      case _ if !ArithExpr.contains(n, i) && (!i.isExplicit) =>
        Solution.subs(i, n)
      case p: Prod => unifyProd(p, i)
      case s: Sum  => unifySum(s, i)
      case _       => error(s"cannot unify $i and $n")
    }
  }

  object natToData {
    def unifyIdent(i: NatToDataIdentifier, n: NatToData)(
      implicit trace: Seq[Constraint]
    ): Solution = n match {
      case j: NatToDataIdentifier =>
        if (i == j) {
          Solution()
        } else {
          error(s"cannot unify $i and $j, they are both bound")
        }
      case _ => Solution.subs(i, n)
    }
  }

  object natCollection {
    def unifyIdent(i: NatCollectionIdentifier, n: NatCollection)(
        implicit trace: Seq[Constraint]
      ):Solution = n match {
      case j: NatCollectionIdentifier =>
        if (i == j) {
          Solution()
        } else {
          error(s"cannot unify $i and $j, they are both bound")
        }
      case _ => Solution.subs(i,n)
    }
  }

  def occurs(i: DataTypeIdentifier, t: Type): Boolean = t match {
    case FunType(it, ot) => occurs(i, it) || occurs(i, ot)
    case _               => false
  }

}
