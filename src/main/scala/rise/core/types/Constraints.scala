package rise.core.types

import arithexpr.arithmetic.BoolExpr.ArithPredicate
import rise.core.DSL.Type.n2dtFun
import rise.core.{freshName, substitute}
import rise.core.lifting.liftDependentFunctionType
import rise.core.types.InferenceException.error
import rise.core.types.DataType._

import scala.collection.mutable

sealed trait Constraint
case class TypeConstraint(a: ExprType, b: ExprType) extends Constraint {
  override def toString: String = s"$a  ~  $b"
}
case class NatConstraint(a: Nat, b: Nat) extends Constraint {
  override def toString: String = s"$a  ~  $b"
}
case class BoolConstraint(a: arithexpr.arithmetic.BoolExpr,
                          b: arithexpr.arithmetic.BoolExpr) extends Constraint {
  override def toString: String = s"$a  ~  $b"
}
case class AddressSpaceConstraint(a: AddressSpace, b: AddressSpace)
  extends Constraint {
  override def toString: String = s"$a  ~  $b"
}
case class MatrixLayoutConstraint(a: MatrixLayout, b: MatrixLayout)
  extends Constraint {
  override def toString: String = s"$a  ~  $b"
}
case class FragmentTypeConstraint(a: Fragment, b: Fragment)
  extends Constraint {
  override def toString: String = s"$a  ~  $b"
}
case class NatToDataConstraint(a: NatToData, b: NatToData)
  extends Constraint {
  override def toString: String = s"$a  ~  $b"
}
case class DepConstraint[T](kind: Kind[T, _], df: ExprType, arg: T, t: ExprType)
  extends Constraint {
  override def toString: String = s"$df ($arg) ~ $t"
}
case class NatCollectionConstraint(a: NatCollection, b: NatCollection)
  extends Constraint {
  override def toString: String = s"$a ~ $b"
}

object Constraint {
  def canBeSubstituted(preserve: Set[Kind.Identifier], i: Kind.Identifier): Boolean =
    !preserve.contains(i)

  def solve(cs: Seq[Constraint], preserve: Set[Kind.Identifier], trace: Seq[Constraint]): Solution =
  solveRec(cs,  Nil, preserve, trace)
  /* faster but not always enough:
   cs match {
    case Nil => Solution()
    case c +: cs =>
      val s = solveOne(c, trace)
      s ++ solve(s.apply(cs), trace)
  }
  */

  def solveRec(cs: Seq[Constraint], rs: Seq[Constraint], preserve: Set[Kind.Identifier], trace: Seq[Constraint]): Solution = (cs, rs) match {
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
  def solveOne(c: Constraint, preserve : Set[Kind.Identifier], trace: Seq[Constraint]): Solution = {
    implicit val _trace: Seq[Constraint] = trace
    def decomposedPreserve(cs: Seq[Constraint], preserve : Set[Kind.Identifier]) = solve(cs, preserve, c +: trace)
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
            decomposed(Seq(TypeConstraint(ina, inb), TypeConstraint(outa, outb)))
          case (
            DepFunType(NatKind, na: NatIdentifier, ta),
            DepFunType(NatKind, nb: NatIdentifier, tb)
            ) =>
              val n = NatIdentifier(freshName("n"))
              decomposedPreserve(Seq(
                NatConstraint(n, na),
                NatConstraint(n, nb),
                TypeConstraint(ta, tb)
              ), preserve + NatKind.IDWrapper(n) - NatKind.IDWrapper(na) - NatKind.IDWrapper(nb))
          case (
            DepFunType(DataKind, dta: DataTypeIdentifier, ta),
            DepFunType(DataKind, dtb: DataTypeIdentifier, tb)
            ) =>
            val dt = DataTypeIdentifier(freshName("t"))
            decomposedPreserve(Seq(
              TypeConstraint(dt, dta),
              TypeConstraint(dt, dtb),
              TypeConstraint(ta, tb),
            ), preserve + DataKind.IDWrapper(dt) - DataKind.IDWrapper(dta) - DataKind.IDWrapper(dtb))
          case (
            DepFunType(AddressSpaceKind, _: AddressSpaceIdentifier, _),
            DepFunType(AddressSpaceKind, _: AddressSpaceIdentifier, _)
            ) =>
            ???

          case (DepPairType(NatKind, x1: NatIdentifier, t1), DepPairType(NatKind, x2: NatIdentifier, t2)) =>
            val n = NatIdentifier(freshName("n"))
            decomposedPreserve(Seq(
              NatConstraint(n, x1),
              NatConstraint(n, x2),
              TypeConstraint(t1, t2),
            ), preserve + NatKind.IDWrapper(n) - NatKind.IDWrapper(x1) - NatKind.IDWrapper(x2))

          case (DepPairType(NatCollectionKind, x1: NatCollectionIdentifier, t1), DepPairType(NatCollectionKind, x2: NatCollectionIdentifier, t2)) =>
            val n = NatCollectionIdentifier(freshName("n"))
            decomposedPreserve(Seq(
              NatCollectionConstraint(n, x1),
              NatCollectionConstraint(n, x2),
              TypeConstraint(t1, t2),
            ), preserve + NatCollectionKind.IDWrapper(n) - NatCollectionKind.IDWrapper(x1) - NatCollectionKind.IDWrapper(x2))

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


      case DepConstraint(kind, df, arg, t) =>
        df match {
          case _: DepFunType[_, _, _] =>
            val applied = liftDependentFunctionType(kind, df)(arg)
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
            val n = NatIdentifier(freshName("n"))
            decomposedPreserve(Seq(
              NatConstraint(n, x1),
              NatConstraint(n, x2),
              TypeConstraint(dt1, dt2),
            ), preserve + NatKind.IDWrapper(n) - NatKind.IDWrapper(x1) - NatKind.IDWrapper(x2))

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

      case AddressSpaceConstraint(a, b) =>
        (a, b) match {
          case (i: AddressSpaceIdentifier, _) if canBeSubstituted(preserve, AddressSpaceKind.IDWrapper(i)) =>
            Solution.subs(i, b)
          case (_, i: AddressSpaceIdentifier) if canBeSubstituted(preserve, AddressSpaceKind.IDWrapper(i)) =>
            Solution.subs(i, a)
          case _ if a == b                 => Solution()
          case _                           => error(s"cannot unify $a and $b")
        }

      case MatrixLayoutConstraint(a, b) =>
        (a, b) match {
          case (i: MatrixLayoutIdentifier, _) if canBeSubstituted(preserve, MatrixLayoutKind.IDWrapper(i)) =>
            Solution.subs(i, b)
          case (_, i: MatrixLayoutIdentifier) if canBeSubstituted(preserve, MatrixLayoutKind.IDWrapper(i)) =>
            Solution.subs(i, a)
          case _ if a == b                 => Solution()
          case _                           => error(s"cannot unify $a and $b")
        }

      case FragmentTypeConstraint(a, b) =>
        (a, b) match {
          case (i: FragmentIdentifier, _) if canBeSubstituted(preserve, FragmentKind.IDWrapper(i)) =>
            Solution.subs(i, b)
          case (_, i: FragmentIdentifier) if canBeSubstituted(preserve, FragmentKind.IDWrapper(i)) =>
            Solution.subs(i, a)
          case _ if a == b                 => Solution()
          case _                           => error(s"cannot unify $a and $b")
        }
    }
  }
  // scalastyle:on method.length

  def unifyTypeIdent(i: TypeIdentifier, t: ExprType, preserve: Set[Kind.Identifier]): Solution = {
    t match {
      case _ if canBeSubstituted(preserve, TypeKind.IDWrapper(i)) =>
        Solution.subs(i, t)
      case _ if i == t => Solution()
      case i2: TypeIdentifier if canBeSubstituted(preserve, TypeKind.IDWrapper(i2)) =>
        Solution.subs(i2, i)
      case _ =>
        throw new Exception(s"$i cannot be substituted for $t")
    }
  }

  // FIXME: datatypes and types are mixed up
  def unifyDataTypeIdent(i: DataTypeIdentifier, t: DataType, preserve : Set[Kind.Identifier])
                        (implicit trace: Seq[Constraint]): Solution = {
    t match {
      case j: DataTypeIdentifier =>
        if (i == j) {
          Solution()
        } else if (canBeSubstituted(preserve, DataKind.IDWrapper(i))) {
          Solution.subs(i, j)
        } else if (canBeSubstituted(preserve, DataKind.IDWrapper(j))) {
          Solution.subs(j, i)
        } else {
          error(s"cannot unify $i and $j, they are both in $preserve")
        }
      case _ if occurs(i, t) => error(s"circular use: $i occurs in $t")
      case _ =>
        if (canBeSubstituted(preserve, DataKind.IDWrapper(i))) {
          Solution.subs(i, t)
        } else {
          error(s"cannot substitute $i, it is $preserve")
        }
    }
  }

  private object nat {
    import arithexpr.arithmetic._

    def unify(a: Nat, b: Nat, preserve : Set[Kind.Identifier])
             (implicit trace: Seq[Constraint]): Solution = {
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
          if m1 == m2 && canBeSubstituted(preserve, NatKind.IDWrapper(x2)) =>
          val k = NatIdentifier("k", RangeAdd(0, PosInf, 1))
          Solution.subs(x2, k*m1 + x1%m1)
        case (arithexpr.arithmetic.Mod(x2: NatIdentifier, m2),
          arithexpr.arithmetic.Mod(x1, m1))
          if m1 == m2 && canBeSubstituted(preserve, NatKind.IDWrapper(x2)) =>
          val k = NatIdentifier("k", RangeAdd(0, PosInf, 1))
          Solution.subs(x2, k*m1 + x1%m1)
        case _ => error(s"cannot unify $a and $b")
      }
    }

    def freeOccurences(n: Nat, preserve : Set[Kind.Identifier]): Map[NatIdentifier, Integer] = {
      val free_occurrences = mutable
        .Map[NatIdentifier, Integer]()
        .withDefault(_ => 0)
      ArithExpr.visit(n, {
        case v: NatIdentifier if canBeSubstituted(preserve, NatKind.IDWrapper(v)) => free_occurrences(v) += 1
        case _ =>
      })
      free_occurrences.toMap
    }

    // collect free variables with only 1 occurrence
    def potentialPivots(n: Nat, preserve : Set[Kind.Identifier]): Set[NatIdentifier] = {
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
          val k = NatIdentifier(freshName("k"), RangeAdd(0, PosInf, 1))
          Some(Solution.subs(pivot, k*m + value))
        case _               =>
          None
      }
    }

    def tryPivots(n: Nat, value: Nat, preserve : Set[Kind.Identifier])
                 (implicit trace: Seq[Constraint]): Solution = {
      potentialPivots(n, preserve).foreach(pivotSolution(_, n, value) match {
        case Some(s) => return s
        case None    =>
      })
      error(s"could not pivot $n = $value")
    }

    def unifyProd(p: Nat, n: Nat, preserve : Set[Kind.Identifier])
                 (implicit trace: Seq[Constraint]): Solution = {
      // n = p --> 1 = p * (1/n)
      tryPivots(p /^ n, 1, preserve)
    }

    def unifySum(s: Sum, n: Nat, preserve : Set[Kind.Identifier])
                (implicit trace: Seq[Constraint]): Solution = {
      // n = s --> 0 = s + (-n)
      tryPivots(s - n, 0, preserve)
    }

    def unifyIdent(i: NatIdentifier, n: Nat, preserve : Set[Kind.Identifier])
                  (implicit trace: Seq[Constraint]): Solution = n match {
      case j: NatIdentifier =>
        if (i == j) {
          Solution()
        } else if (canBeSubstituted(preserve, NatKind.IDWrapper(i))) {
          Solution.subs(i, j)
        } else if (canBeSubstituted(preserve, NatKind.IDWrapper(j))) {
          Solution.subs(j, i)
        } else {
          error(s"cannot unify $i and $j")
        }
      case fx: NatToNatApply => unifyApply(fx, i, preserve)
      case _ if !ArithExpr.contains(n, i) && (canBeSubstituted(preserve, NatKind.IDWrapper(i))) =>
        Solution.subs(i, n)
      case p: Prod => unifyProd(p, i, preserve)
      case s: Sum  => unifySum(s, i, preserve)
      case _       => error(s"cannot unify $i and $n")
    }

    def unifyApply(apply: NatToNatApply, nat: Nat, preserve: Set[Kind.Identifier])
                  (implicit trace: Seq[Constraint]): Solution = {
      val NatToNatApply(f1, n1) = apply
      nat match {
        case NatToNatApply(f2, n2) =>
          natToNat.unify(f1, f2, preserve) ++ unify(n1, n2, preserve)
        case _ => (f1, n1) match {
          case (f1: NatToNatIdentifier, n1: NatIdentifier) =>
            val freshVar = NatIdentifier(freshName("n"))
            val lambda = NatToNatLambda(freshVar, substitute.natInNat(freshVar, n1, nat))
            Solution.subs(f1, lambda)
          case _ => ???
        }
      }
    }
  }

  private object bool {
    import arithexpr.arithmetic._

    def unify(a: BoolExpr, b: BoolExpr, preserve : Set[Kind.Identifier])
             (implicit trace: Seq[Constraint]): Solution = {
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
    def unifyIdent(i: NatToDataIdentifier, n: NatToData)
                  (implicit trace: Seq[Constraint]): Solution = n match {
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
    def unify(f1: NatToNat, f2: NatToNat, preserve: Set[Kind.Identifier])
             (implicit trace: Seq[Constraint]): Solution = f1 match {
      case id1: NatToNatIdentifier => Solution.subs(id1, f2)
      case NatToNatLambda(x1, body1) => f2 match {
        case id2: NatToNatIdentifier => Solution.subs(id2, f1)
        case NatToNatLambda(x2, body2) =>
          val n = NatIdentifier(freshName("n"))
          nat.unify(
            substitute.natInNat(n, `for` = x1, body1),
            substitute.natInNat(n, `for`=x2, body2),
            preserve + NatKind.IDWrapper(n))
      }
    }
  }

  object natCollection {
    def unifyIdent(i: NatCollectionIdentifier, n: NatCollection, preserve : Set[Kind.Identifier])
                  (implicit trace: Seq[Constraint]): Solution = n match {
      case j: NatCollectionIdentifier =>
        if (i == j) {
          Solution()
        } else {
          error(s"cannot unify $i and $j, they are both bound")
        }
      case _ => Solution.subs(i,n)
    }
  }

  def occurs(i: DataTypeIdentifier, t: ExprType): Boolean = t match {
    case FunType(it, ot) => occurs(i, it) || occurs(i, ot)
    case _               => false
  }

}