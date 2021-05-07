package rise.core.types

import util.monads._
import arithexpr.arithmetic.BoolExpr.ArithPredicate
import rise.core.DSL.Type.n2dtFun
import rise.core.traverse._
import rise.core.{freshName, substitute}
import rise.core.lifting.liftDependentFunctionType
import rise.core.types.Flags.ExplicitDependence
import rise.core.types.InferenceException.error

import scala.collection.mutable

sealed trait Constraint
case class TypeConstraint(a: Type, b: Type) extends Constraint {
  override def toString: String = s"$a  ~  $b"
}
case class NatConstraint(a: Nat, b: Nat) extends Constraint {
  override def toString: String = s"$a  ~  $b"
}
case class BoolConstraint(a: arithexpr.arithmetic.BoolExpr,
                          b: arithexpr.arithmetic.BoolExpr) extends Constraint {
  override def toString: String = s"$a  ~  $b"
}
case class MatrixLayoutConstraint(a: MatrixLayout, b: MatrixLayout)
  extends Constraint {
  override def toString: String = s"$a  ~  $b"
}
case class FragmentTypeConstraint(a: FragmentKind, b: FragmentKind)
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
  def canBeSubstituted(preserve: Set[String], i: Kind.Identifier with Kind.Explicitness): Boolean =
    !(preserve.contains(i.name) || i.isExplicit)
  def canBeSubstituted(preserve: Set[String], i: TypeIdentifier): Boolean =
    !preserve.contains(i.name)

  def solve(cs: Seq[Constraint], preserve: Set[String], trace: Seq[Constraint])
     (implicit explDep: Flags.ExplicitDependence): Solution =
  solveRec(cs, Nil, preserve, trace)
  /* faster but not always enough:
   cs match {
    case Nil => Solution()
    case c +: cs =>
      val s = solveOne(c, trace)
      s ++ solve(s.apply(cs), trace)
  }
  */

  def solveRec(cs: Seq[Constraint], rs: Seq[Constraint], preserve: Set[String], trace: Seq[Constraint])
              (implicit explDep: Flags.ExplicitDependence): Solution = (cs, rs) match {
    case (Nil, Nil) => Solution()
    case (Nil, _) => error(s"could not solve constraints ${rs}")(trace)
    case (c +: cs, _) =>
      val s = try { solveOne(c, preserve, trace) }
              catch { case e: InferenceException =>
                println(e.msg)
                return solveRec(cs, rs :+ c, preserve, trace) }
      s ++ solve(s.apply(rs ++ cs), preserve, trace)
  }

  // scalastyle:off method.length
  def solveOne(c: Constraint, preserve: Set[String], trace: Seq[Constraint]) (implicit explDep: Flags.ExplicitDependence): Solution = {
    implicit val _trace: Seq[Constraint] = trace
    def decomposed(cs: Seq[Constraint]) = solve(cs, preserve, c +: trace)

    c match {
      case TypeConstraint(a, b) =>
        (a, b) match {
          case (TypePlaceholder, _) => Solution()
          case (_, TypePlaceholder) => Solution()
          case (i: TypeIdentifier, _) => unifyTypeIdent(i, b, preserve)
          case (_, i: TypeIdentifier) => unifyTypeIdent(i, a, preserve)
          case (i: DataTypeIdentifier, dt: DataType) =>
            unifyDataTypeIdent(i, dt, preserve)
          case (dt: DataType, i: DataTypeIdentifier) =>
            unifyDataTypeIdent(i, dt, preserve)
          case (_: ScalarType | _: NatType.type |
                _: IndexType  | _: VectorType,
          _: ScalarType | _: NatType.type |
          _: IndexType  | _: VectorType)
            if a =~= b => Solution()
          case (IndexType(sa), IndexType(sb)) =>
            decomposed(Seq(NatConstraint(sa, sb)))
          case (ArrayType(sa, ea), ArrayType(sb, eb)) =>
            decomposed(Seq(NatConstraint(sa, sb), TypeConstraint(ea, eb)))
          case (VectorType(sa, ea), VectorType(sb, eb)) =>
            decomposed(Seq(NatConstraint(sa, sb), TypeConstraint(ea, eb)))
          case (FragmentType(rowsa, columnsa, d3a, dta, fragTypea, layouta), FragmentType(rowsb, columnsb, d3b, dtb, fragTypeb, layoutb)) =>
            decomposed(Seq(NatConstraint(rowsa, rowsb), NatConstraint(columnsa, columnsb), NatConstraint(d3a, d3b),
              TypeConstraint(dta, dtb), FragmentTypeConstraint(fragTypea, fragTypeb), MatrixLayoutConstraint(layouta, layoutb)))
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
            explDep match {
              case ExplicitDependence.On =>
                val n = NatIdentifier(freshName("n"), isExplicit = true)
                /** Note(federico):
                  * This step recurses in both functions and makes dependence between type
                  * variables and n explicit (by replacing type variables with NatToData/NatToNat).
                  *
                  * Perhaps this can be moved away from constraint solving, and pulled up in the
                  * initial constrain-types phase?
                  */
                val (nTa, nTaSub) = dependence.explicitlyDependent(
                  substitute.natInType(n, `for`=na, ta), n, preserve)
                val (nTb, nTbSub) = dependence.explicitlyDependent(
                  substitute.natInType(n, `for`= nb, tb), n, preserve)
                nTaSub ++ nTbSub ++ decomposed(
                    Seq(
                      NatConstraint(n, na.asImplicit),
                      NatConstraint(n, nb.asImplicit),
                      TypeConstraint(nTa, nTb)
                    ))
              case ExplicitDependence.Off =>
                val n = NatIdentifier(freshName("n"), isExplicit = true)
                decomposed(
                  Seq(
                    NatConstraint(n, na.asImplicit),
                    NatConstraint(n, nb.asImplicit),
                    TypeConstraint(ta, tb)
                  )
                )
            }
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
            val r = n2dtFun(fresh => substitute.natInDataType(fresh, n, dt))
            Solution.subs(f, r)
          case (
            dt: DataType,
            NatToDataApply(f: NatToDataIdentifier, n: NatIdentifier)
            ) =>
            val r = n2dtFun(fresh => substitute.natInDataType(fresh, n, dt))
            Solution.subs(f, r)
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
            val applied = liftDependentFunctionType(df)(arg)
            decomposed(Seq(TypeConstraint(applied, t)))
          case _ =>
            error(s"expected a dependent function type, but got $df")
        }

      case NatConstraint(a, b) => nat.unify(a, b, preserve)
      case BoolConstraint(a, b) => bool.unify(a, b, preserve)
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
          case (i: NatCollectionIdentifier, _) => natCollection.unifyIdent(i, b, preserve)
          case (_, i: NatCollectionIdentifier) => natCollection.unifyIdent(i, b, preserve)
          case _ if a == b                    => Solution()
          case (NatCollectionFromArray(e1), NatCollectionFromArray(e2)) =>
            // What to do here???
            ???
        }

      case MatrixLayoutConstraint(a, b) =>
        (a, b) match {
          case (i: MatrixLayoutIdentifier, _) if canBeSubstituted(preserve, i) =>
            Solution.subs(i, b)
          case (_, i: MatrixLayoutIdentifier) if canBeSubstituted(preserve, i) =>
            Solution.subs(i, a)
          case _ if a == b                 => Solution()
          case _                           => error(s"cannot unify $a and $b")
        }

      case FragmentTypeConstraint(a, b) =>
        (a, b) match {
          case (i: FragmentKindIdentifier, _) if canBeSubstituted(preserve, i) =>
            Solution.subs(i, b)
          case (_, i: FragmentKindIdentifier) if canBeSubstituted(preserve, i) =>
            Solution.subs(i, a)
          case _ if a == b                 => Solution()
          case _                           => error(s"cannot unify $a and $b")
        }
    }
  }
  // scalastyle:on method.length

  def unifyTypeIdent(i: TypeIdentifier, t: Type, preserve: Set[String]): Solution = {
    t match {
      case _ if canBeSubstituted(preserve, i) =>
        Solution.subs(i, t)
      case _ if i == t => Solution()
      case i2: TypeIdentifier if canBeSubstituted(preserve, i2) =>
        Solution.subs(i2, i)
      case _ =>
        throw new Exception(s"$i cannot be substituted for $t")
    }
  }

  // FIXME: datatypes and types are mixed up
  def unifyDataTypeIdent(i: DataTypeIdentifier, t: DataType, preserve: Set[String])
                        (implicit trace: Seq[Constraint]): Solution = {
    t match {
      case j: DataTypeIdentifier =>
        if (i == j) {
          Solution()
        } else if (canBeSubstituted(preserve, i)) {
          Solution.subs(i, j)
        } else if (canBeSubstituted(preserve, j)) {
          Solution.subs(j, i)
        } else {
          error(s"cannot unify $i and $j, they are both explicit or in $preserve")
        }
      case _ if occurs(i, t) => error(s"circular use: $i occurs in $t")
      case _ =>
        if (canBeSubstituted(preserve, i)) {
          Solution.subs(i, t)
        } else {
          error(s"cannot substitute $i, it is explicit")
        }
    }
  }

  private object nat {
    import arithexpr.arithmetic._

    def unify(a: Nat, b: Nat, preserve: Set[String])
             (implicit trace: Seq[Constraint], explDep: Flags.ExplicitDependence): Solution = {
      def decomposed(cs: Seq[Constraint]) = solve(cs, preserve, NatConstraint(a, b) +: trace)
      (a, b) match {
        case (i: NatIdentifier, _) => nat.unifyIdent(i, b, preserve)
        case (_, i: NatIdentifier) => nat.unifyIdent(i, a, preserve)
        case (app: NatToNatApply, _) => nat.unifyApply(app, b, preserve)
        case (_, app: NatToNatApply) => nat.unifyApply(app, a, preserve)
        case _ if a == b => Solution()
        // case _ if !nat.potentialPivots(a).isEmpty => nat.tryPivots(a, b)
        // case _ if !nat.potentialPivots(b).isEmpty => nat.tryPivots(b, a)
        case (Sum(Cst(k1) :: IfThenElse(c1, t1, e1) :: Nil),
          Sum(Cst(k2) :: IfThenElse(c2, t2, e2) :: Nil)) if k1 == k2 =>
          decomposed(Seq(
            NatConstraint(t1, t2), NatConstraint(e1, e2), BoolConstraint(c1, c2)
          ))
        case (IfThenElse(c1, t1, e1), IfThenElse(c2, t2, e2)) =>
          decomposed(Seq(
            NatConstraint(t1, t2), NatConstraint(e1, e2), BoolConstraint(c1, c2)
          ))
        case (s: arithexpr.arithmetic.Sum, _) => nat.unifySum(s, b, preserve)
        case (_, s: arithexpr.arithmetic.Sum) => nat.unifySum(s, a, preserve)
        case (p: arithexpr.arithmetic.Prod, _) => nat.unifyProd(p, b, preserve)
        case (_, p: arithexpr.arithmetic.Prod) => nat.unifyProd(p, a, preserve)
        case (p: arithexpr.arithmetic.Pow, _) => nat.unifyProd(p, b, preserve)
        case (_, p: arithexpr.arithmetic.Pow) => nat.unifyProd(p, a, preserve)
        case (p: arithexpr.arithmetic.IntDiv, _) => nat.unifyProd(p, b, preserve)
        case (_, p: arithexpr.arithmetic.IntDiv) => nat.unifyProd(p, a, preserve)
        case (arithexpr.arithmetic.Mod(x1, m1),
          arithexpr.arithmetic.Mod(x2: NatIdentifier, m2))
          if m1 == m2 && canBeSubstituted(preserve, x2) =>
          val k = NatIdentifier("k", RangeAdd(0, PosInf, 1))
          Solution.subs(x2, k*m1 + x1%m1)
        case (arithexpr.arithmetic.Mod(x2: NatIdentifier, m2),
          arithexpr.arithmetic.Mod(x1, m1))
          if m1 == m2 && canBeSubstituted(preserve, x2) =>
          val k = NatIdentifier("k", RangeAdd(0, PosInf, 1))
          Solution.subs(x2, k*m1 + x1%m1)
        case _ => error(s"cannot unify $a and $b")
      }
    }

    def freeOccurences(n: Nat, preserve: Set[String]): Map[NatIdentifier, Integer] = {
      val free_occurrences = mutable
        .Map[NatIdentifier, Integer]()
        .withDefault(_ => 0)
      ArithExpr.visit(n, {
        case v: NatIdentifier if canBeSubstituted(preserve, v) => free_occurrences(v) += 1
        case _ =>
      })
      free_occurrences.toMap
    }

    // collect free variables with only 1 occurrence
    def potentialPivots(n: Nat, preserve: Set[String]): Set[NatIdentifier] = {
      freeOccurences(n, preserve).foldLeft(Set[NatIdentifier]())({
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
        case Mod(p, m) if p == pivot =>
          val k = NatIdentifier("k", RangeAdd(0, PosInf, 1))
          Some(Solution.subs(pivot, k*m + value))
        case _               =>
          None
      }
    }

    def tryPivots(n: Nat, value: Nat, preserve: Set[String])(implicit trace: Seq[Constraint]): Solution = {
      potentialPivots(n, preserve).foreach(pivotSolution(_, n, value) match {
        case Some(s) => return s
        case None    =>
      })
      error(s"could not pivot $n = $value")
    }

    def unifyProd(p: Nat, n: Nat, preserve: Set[String])(implicit trace: Seq[Constraint]): Solution = {
      // n = p --> 1 = p * (1/n)
      tryPivots(p /^ n, 1, preserve)
    }

    def unifySum(s: Sum, n: Nat, preserve: Set[String])(implicit trace: Seq[Constraint]): Solution = {
      // n = s --> 0 = s + (-n)
      tryPivots(s - n, 0, preserve)
    }

    def unifyIdent(i: NatIdentifier, n: Nat, preserve: Set[String])(
      implicit trace: Seq[Constraint], explDep: Flags.ExplicitDependence
    ): Solution = n match {
      case j: NatIdentifier =>
        if (i == j) {
          Solution()
        } else if (canBeSubstituted(preserve, i)) {
          Solution.subs(i, j)
        } else if (canBeSubstituted(preserve, j)) {
          Solution.subs(j, i)
        } else {
          error(s"cannot unify $i and $j")
        }
      case fx: NatToNatApply => unifyApply(fx, i, preserve)
      case _ if !ArithExpr.contains(n, i) && (canBeSubstituted(preserve, i)) =>
        Solution.subs(i, n)
      case p: Prod => unifyProd(p, i, preserve)
      case s: Sum  => unifySum(s, i, preserve)
      case _       => error(s"cannot unify $i and $n")
    }

    def unifyApply(apply: NatToNatApply, nat: Nat, preserve: Set[String])(
      implicit trace: Seq[Constraint], explDep: Flags.ExplicitDependence
    ): Solution = {
      val NatToNatApply(f1, n1) = apply
      nat match {
        case NatToNatApply(f2, n2) =>
          natToNat.unify(f1, f2, preserve) ++ unify(n1, n2, preserve)
        case _ => (f1, n1) match {
          case (f1: NatToNatIdentifier, n1: NatIdentifier) =>
            val freshVar = NatIdentifier(freshName("n"), isExplicit = true)
            val lambda = NatToNatLambda(freshVar, substitute.natInNat(freshVar, n1, nat))
            Solution.subs(f1, lambda)
          case _ => ???
        }
      }
    }
  }

  private object bool {
    import arithexpr.arithmetic._

    def unify(a: BoolExpr, b: BoolExpr, preserve: Set[String])(
      implicit trace: Seq[Constraint], explDep: Flags.ExplicitDependence
    ): Solution = {
      def decomposed(cs: Seq[Constraint]) = solve(cs, preserve, BoolConstraint(a, b) +: trace)
      (a, b) match {
        case _ if a == b => Solution()
        case (ArithPredicate(lhs1, rhs1, op1), ArithPredicate(lhs2, rhs2, op2)) if op1 == op2 =>
          decomposed(Seq(NatConstraint(lhs1, lhs2), NatConstraint(rhs1, rhs2)))
        case _ => error(s"cannot unify $a and $b")
      }
    }
  }

  object natToData {
    def unifyIdent(i: NatToDataIdentifier, n: NatToData)(implicit trace: Seq[Constraint]): Solution = n match {
      case j: NatToDataIdentifier =>
        if (i == j) {
          Solution()
        } else {
          error(s"cannot unify $i and $j, they are both bound")
        }
      case _ => Solution.subs(i, n)
    }
  }

  object natToNat {
    def unify(f1: NatToNat, f2: NatToNat, preserve: Set[String])(
      implicit trace: Seq[Constraint], explDep: Flags.ExplicitDependence
    ): Solution = f1 match {
      case id1: NatToNatIdentifier => Solution.subs(id1, f2)
      case NatToNatLambda(x1, body1) => f2 match {
        case id2: NatToNatIdentifier => Solution.subs(id2, f1)
        case NatToNatLambda(x2, body2) =>
          val n = NatIdentifier(freshName("n"), isExplicit = true)
          nat.unify(
            substitute.natInNat(n, `for` = x1, body1),
            substitute.natInNat(n, `for`=x2, body2),
            preserve)
      }
    }
  }

  object natCollection {
    def unifyIdent(i: NatCollectionIdentifier, n: NatCollection, preserve: Set[String])(
        implicit trace: Seq[Constraint]
      ): Solution = n match {
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

object dependence {
  /*
   * Given a type t which is in the scope of a natIdentifier depVar,
   * explicitly represent the dependence by replacing identifiers in t
   * with applied nat-to-X functions.
   */
  def explicitlyDependent(t: Type, depVar: NatIdentifier, preserve: Set[String]): (Type, Solution) = {
    val visitor = new PureAccumulatorTraversal[Seq[Solution]] {
      override val accumulator = SeqMonoid

      override def nat: Nat => Pair[Nat] = {
        case n2n@NatToNatApply(_, n) if n == depVar => return_(n2n : Nat)
        case ident: NatIdentifier
          if ident != depVar && Constraint.canBeSubstituted(preserve, ident) =>
          val sol = Solution.subs(ident, NatToNatApply(NatToNatIdentifier(freshName("nnf")), depVar))
          accumulate(Seq(sol))(ident.asImplicit : Nat)
        case n => super.nat(n)
      }

      override def `type`[T <: Type] : T => Pair[T] = {
        case n2d@NatToDataApply(_, x) if x == depVar => return_(n2d : T)
        case ident@TypeIdentifier(i) =>
          val application = NatToDataApply(NatToDataIdentifier(freshName("nnf")), depVar)
          val sol = Solution.subs(ident, application)
          accumulate(Seq(sol))(ident.asInstanceOf[T])
        case e => super.`type`(e)
      }

      def apply(t: Type): (Type, Solution) = {
        val (sols, rewrittenT) = traverse(t, this)
        val solution = sols.foldLeft(Solution())(_ ++ _)
        (solution.apply(rewrittenT), solution)
      }
    }

    visitor(t)
  }
}
