package rise.core.types

import rise.core.TypeLevelDSL.n2dtFun
import rise.core.{freshName, substitute}
import rise.core.lifting.liftDependentFunctionType
import rise.core.types.Flags.ExplicitDependence
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

case class NatCollectionToDataConstraint(a: NatCollectionToData, b: NatCollectionToData)
  extends Constraint {
  override def toString: String = s"$a ~ $b"
}

object Constraint {
  def solve(cs: Seq[Constraint], trace: Seq[Constraint])
     (implicit explDep: Flags.ExplicitDependence): Solution = cs match {
    case Nil => Solution()
    case c +: cs =>
      val s = solveOne(c, trace)
      s ++ solve(s.apply(cs), trace)
  }

  // scalastyle:off method.length
  def solveOne(c: Constraint, trace: Seq[Constraint])
    (implicit explDep: Flags.ExplicitDependence): Solution = {
    implicit val _trace: Seq[Constraint] = trace
    def decomposed(cs: Seq[Constraint]) =
      solve(cs, c +: trace)


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
            DepFunType(ns: NatCollectionIdentifier, ta),
            DepFunType(ms: NatCollectionIdentifier, tb),
            ) =>
            val n = NatCollectionIdentifier(freshName("ns"), isExplicit = true)

            decomposed(
              Seq(
                NatCollectionConstraint(n, ns.asImplicit),
                NatCollectionConstraint(n, ms.asImplicit),
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
            DepFunType(fdta: NatCollectionToDataIdentifier, ta),
            DepFunType(fdtb: NatCollectionToDataIdentifier, tb)
            ) =>
            val fdtc = NatCollectionToDataIdentifier(freshName("ns2d"), isExplicit = true)
            decomposed(
              Seq(
                NatCollectionToDataConstraint(fdtc, fdta.asImplicit),
                NatCollectionToDataConstraint(fdtc, fdtb.asImplicit),
                TypeConstraint(ta, tb)
              )
            )

          case (
            DepPairType(fdt1:NatToData),
            DepPairType(fdt2:NatToData)
            ) =>
            decomposed(Seq(NatToDataConstraint(fdt1, fdt2)))

          case (
            DepPairType(fdt1:NatCollectionToData),
            DepPairType(fdt2:NatCollectionToData)
            ) =>
            decomposed(Seq(NatCollectionToDataConstraint(fdt1, fdt2)))

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
            Solution.subs(f, n2dtFun(n.range)(
              fresh => substitute.natInDataType(fresh, n, dt)
            ))
          case (
            dt: DataType,
            NatToDataApply(f: NatToDataIdentifier, n: NatIdentifier)
            ) =>
            Solution.subs(f, n2dtFun(n.range)(
              fresh => substitute.natInDataType(fresh, n, dt)
            ))
          case (_: NatToDataApply, dt: DataType) =>
            Solution.subs(a, dt) // substitute apply by data type
          case (dt: DataType, _: NatToDataApply) =>
            Solution.subs(b, dt) // substitute apply by data type

            // TODO(fix properly)
          case (NatCollectionToDataApply(f:NatCollectionToDataIdentifier, ns), dt:DataType) =>
            // Do the not-really-valid thing where we attempt to invert the function
            val lambda = NatCollectionToDataLambda(ns.asInstanceOf[NatCollectionIdentifier], dt)
            Solution.subs(f, lambda)


          case (TSub(x1, y1, dt1), TSub(x2, y2, dt2)) if(x1 == x2) && y1 == y2 =>
            decomposed(Seq(
              TypeConstraint(dt1, dt2)
            ))

          case _ =>
            error(s"cannot unify $a and $b")
        }


      case DepConstraint(df, arg, t) =>
        df match {
          case df: DepFunType[_, _] =>
            val tc = TypeConstraint(liftDependentFunctionType(df)(arg), t)
            val subs = df.x match {
              case x:NatCollectionIdentifier =>
                Seq(NatCollectionConstraint(x.asImplicit, arg.asInstanceOf[NatCollection]))
              case _ => Seq()
            }
            decomposed(
              subs ++ Seq(tc)
            )
          case _ =>
            error(s"expected a dependent function type, but got $df")
        }

      case NatConstraint(a, b) => nat.unify(a, b)
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
          case (_, i: NatCollectionIdentifier) => natCollection.unifyIdent(i, a)
          case _ if a == b                    => Solution()
        }

      case NatCollectionToDataConstraint(a,b) =>
        (a,b) match {
          case (i: NatCollectionToDataIdentifier, _) => ns2d.unifyIdent(i, b)
          case (_, i: NatCollectionToDataIdentifier) => ns2d.unifyIdent(i, a)
          case (NatCollectionToDataLambda(x1, dt1), NatCollectionToDataLambda(x2, dt2)) =>
            val n = NatCollectionIdentifier(freshName("ns"), isExplicit = true)
            decomposed(Seq(
              NatCollectionConstraint(n, x1.asImplicit),
              NatCollectionConstraint(n, x2.asImplicit),
              TypeConstraint(dt1, dt2)
            ))
          case _ if a == b                    => Solution()
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

    def unify(a: Nat, b: Nat)(implicit trace: Seq[Constraint]): Solution = {
      (a, b) match {
        case (i: NatIdentifier, _) => nat.unifyIdent(i, b)
        case (_, i: NatIdentifier) => nat.unifyIdent(i, a)
        case (app: NatToNatApply, _) =>
          nat.unifyApply(app, b)
        case (_, app: NatToNatApply) =>
          nat.unifyApply(app, a)
        case _ if a == b => Solution()
        // case _ if !nat.potentialPivots(a).isEmpty => nat.tryPivots(a, b)
        // case _ if !nat.potentialPivots(b).isEmpty => nat.tryPivots(b, a)
        case (s: arithexpr.arithmetic.Sum, _) => nat.unifySum(s, b)
        case (_, s: arithexpr.arithmetic.Sum) => nat.unifySum(s, a)
        case (p: arithexpr.arithmetic.Prod, _) => nat.unifyProd(p, b)
        case (_, p: arithexpr.arithmetic.Prod) => nat.unifyProd(p, a)
        case (p: arithexpr.arithmetic.Pow, _) => nat.unifyProd(p, b)
        case (_, p: arithexpr.arithmetic.Pow) => nat.unifyProd(p, a)
        case (p: arithexpr.arithmetic.IntDiv, _) => nat.unifyProd(p, b)
        case (_, p: arithexpr.arithmetic.IntDiv) => nat.unifyProd(p, a)
        case _ => error(s"cannot unify $a and $b")
      }
    }

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
      tryPivots(s - n, 0)
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
          error(s"cannot unify $i and $j")
        }
      case fx: NatToNatApply => unifyApply(fx, i)
      case _ if !ArithExpr.contains(n, i) && (!i.isExplicit) =>
        Solution.subs(i, n)
      case p: Prod => unifyProd(p, i)
      case s: Sum  => unifySum(s, i)
      case _       => error(s"cannot unify $i and $n")
    }

    def unifyApply(apply: NatToNatApply, nat: Nat)(
      implicit trace: Seq[Constraint]
    ): Solution = {
      val NatToNatApply(f1, n1) = apply
      nat match {
        case NatToNatApply(f2, n2) =>
          natToNat.unify(f1, f2) ++ unify(n1, n2)
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

  object natToNat {
    def unify(f1: NatToNat, f2: NatToNat)(implicit trace: Seq[Constraint]): Solution = f1 match {
      case id1: NatToNatIdentifier => Solution.subs(id1, f2)
      case NatToNatLambda(x1, body1) => f2 match {
        case id2: NatToNatIdentifier => Solution.subs(id2, f1)
        case NatToNatLambda(x2, body2) =>
          val n = NatIdentifier(freshName("n"))
          nat.unify(
            substitute.natInNat(n, `for` = x1, body1),
            substitute.natInNat(n, `for`=x2, body2)
          )
      }
    }
  }

  object natCollection {
    def unifyIdent(i: NatCollectionIdentifier, n: NatCollection)(
      implicit trace: Seq[Constraint]
    ): Solution = n match {
      case j: NatCollectionIdentifier =>
        if (i == j) {
          Solution()
        } else if (!i.isExplicit) {
          Solution.subs(i, j)
        } else if (!j.isExplicit) {
          Solution.subs(j, i)
        } else {
          error(s"cannot unify $i and $j")
        }
      case _       => error(s"cannot unify $i and $n")
    }
  }

  object ns2d {
    def unifyIdent(i: NatCollectionToDataIdentifier, n: NatCollectionToData)(
      implicit trace: Seq[Constraint]
    ): Solution = n match {
      case j: NatCollectionToDataIdentifier =>
        if (i == j) {
          Solution()
        } else if (!i.isExplicit) {
          Solution.subs(i, j)
        } else if (!j.isExplicit) {
          Solution.subs(j, i)
        } else {
          error(s"cannot unify $i and $j")
        }
      case l:NatCollectionToDataLambda =>
        Solution.subs(i, l)
    }
  }

  def occurs(i: DataTypeIdentifier, t: Type): Boolean = t match {
    case FunType(it, ot) => occurs(i, it) || occurs(i, ot)
    case _               => false
  }

}

object dependence {

  import rise.core.traversal._

  /*
   * Given a type t which is in the scope of a natIdentifier depVar,
   * explicitly represent the dependence by replacing identifiers in t
   * with applied nat-to-X functions.
   */
  def explicitlyDependent(t: Type, depVar: NatIdentifier): (Type, Solution) = {
    val visitor = new Visitor {
      private val sols = Seq.newBuilder[Solution]

      override def visitNat(ae: Nat): Result[Nat] = ae match {
        case ident: NatIdentifier if ident != depVar && !ident.isExplicit =>
          sols += Solution.subs(ident, NatToNatApply(NatToNatIdentifier(freshName("nnf")), depVar))
          Continue(ident.asImplicit, this)
        case n2n@NatToNatApply(_, n) if n == depVar =>
          Continue(n2n, this)
        case other => Continue(other, this)
      }

      override def visitType[T <: Type](t: T): Result[T] = t match {
        case ident@TypeIdentifier(_) =>
          val application = NatToDataApply(NatToDataIdentifier(freshName("nnf")), depVar)
          sols += Solution.subs(ident, application)
          Continue(ident.asInstanceOf[T], this)
        case n2d@NatToDataApply(_, x) if x == depVar =>
          Continue(n2d, this)
        case other => Continue(other, this)
      }

      def apply(t: Type): (Type, Solution) = {
        val rewrittenT = types.DepthFirstGlobalResult(t, this).value
        val solution = this.sols.result().foldLeft(Solution())(_ ++ _)
        (solution.apply(rewrittenT), solution)
      }
    }

    visitor(t)
  }
}
