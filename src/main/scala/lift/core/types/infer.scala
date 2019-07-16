package lift.core.types

import lift.arithmetic.NamedVar
import lift.core._
import lift.core.lifting._

import scala.collection.mutable

case class InferenceException(msg: String) extends Exception {
  override def toString = s"inference exception: $msg"
}

object infer {
  def apply(e: Expr): TypedExpr = {
    val constraints = mutable.Set[Constraint]()
    val typed_e = constrainTypes(e, constraints, mutable.Map())
    implicit val (boundT, boundN) = boundIdentifiers(typed_e)
    constraints.toSet.foreach(println)
    val solution = solve(constraints.toSet)
    solution(typed_e).asInstanceOf[TypedExpr]
  }

  def error(msg: String): Nothing =
    throw InferenceException(msg)

  trait Constraint
  case class TypeConstraint(a: Type, b: Type) extends Constraint {
    override def toString: String = s"$a  ~  $b"
  }
  case class NatConstraint(a: Nat, b: Nat) extends Constraint {
    override def toString: String = s"$a  ~  $b"
  }

  def constrainTypes(expr: Expr,
                     constraints: mutable.Set[Constraint],
                     identifierT: scala.collection.mutable.Map[Identifier, Type]
                    ): TypedExpr = {
    def fresh(): Type = DataTypeIdentifier(freshName("_t"))
    def typed(e: Expr): TypedExpr = constrainTypes(e, constraints, identifierT)

    expr match {
      case i: Identifier =>
        val t = identifierT
          .getOrElse(i, error(s"$i has no type in the environment"))
        TypedExpr(i, t)

      case Lambda(x, e) =>
        val xt = fresh()
        identifierT update (x, xt)
        val te = typed(e)
        identifierT remove x
        TypedExpr(Lambda(x, te), FunctionType(xt, te.t))

      case Apply(f, e) =>
        val tf = typed(f)
        val te = typed(e)
        val ot = fresh()
        constraints += TypeConstraint(tf.t, FunctionType(te.t, ot))
        TypedExpr(Apply(tf, te), ot)

      case DepLambda(x, e) => x match {
        case n: NatIdentifier =>
          val te = typed(e)
          TypedExpr(NatDepLambda(n, te), NatDependentFunctionType(n, te.t))
        case dt: DataTypeIdentifier =>
          val te = typed(e)
          TypedExpr(TypeDepLambda(dt, te), TypeDependentFunctionType(dt, te.t))
      }

      case DepApply(f, x) => x match {
        case n: Nat =>
          val tf = typed(f)
          TypedExpr(NatDepApply(tf, n), liftDependentFunctionType[NatKind](tf.t)(n))
        case dt: DataType =>
          val tf = typed(f)
          TypedExpr(TypeDepApply(tf, dt), liftDependentFunctionType[DataKind](tf.t)(dt))
      }

      case l: Literal => TypedExpr(l, l.d.dataType)

      case i: Index => TypedExpr(i, IndexType(i.size))

      case n: NatExpr => TypedExpr(n, NatType)

      case TypedExpr(e, t) =>
        val te = typed(e)
        constraints += TypeConstraint(te.t, t)
        te

      case p: Primitive => TypedExpr(p, p.t)
    }
  }

  def boundIdentifiers(expr: TypedExpr)
  : (mutable.Set[DataTypeIdentifier], mutable.Set[NamedVar]) = {
    import traversal.{Result, Continue}

    val boundT = mutable.Set[DataTypeIdentifier]()
    val boundN = mutable.Set[NamedVar]()

    case class Visitor() extends traversal.Visitor {
      override def apply(e: Expr): Result[Expr] = {
        e match {
          case DepLambda(x: NatIdentifier, _) => boundN += x
          case DepLambda(x: DataTypeIdentifier, _) => boundT += x
          case _ =>
        }
        Continue(e, this)
      }

      override def apply[T <: Type](t: T): Result[T] = {
        val r = traversal.types.DepthFirstLocalResult(t, new traversal.Visitor() {
          override def apply[U <: Type](t: U): Result[U] = {
            t match {
              case DependentFunctionType(x: NatIdentifier, _) => boundN += x
              case DependentFunctionType(x: DataTypeIdentifier, _) => boundT += x
              case _ =>
            }
            Continue(t, this)
          }
        })
        Continue(r, this)
      }
    }

    traversal.DepthFirstLocalResult(expr, Visitor())
    (boundT, boundN)
  }

  object Solution {
    def apply(): Solution = Solution(Map(), Map())
    def subs(ta: Type, tb: Type): Solution = Solution(Map(ta -> tb), Map())
    def subs(na: NamedVar, nb: Nat): Solution = Solution(Map(), Map(na -> nb))
  }

  case class Solution(ts: Map[Type, Type],
                      ns: Map[NamedVar, Nat]) {
    def apply(e: Expr): Expr = {
      val sol = this
      traversal.DepthFirstLocalResult(e, new traversal.Visitor {
        import traversal.{Result, Continue}
        override def apply(ae: Nat): Result[Nat] = Continue(sol(ae), this)
        override def apply[T <: Type](t: T): Result[T] = Continue(sol(t).asInstanceOf[T], this)
      })
    }

    def apply(t: Type): Type = {
      val t2 = ts.foldLeft(t) { case (result, (ta, tb)) =>
        substitute(tb, `for` = ta, in = result)
      }
      ns.foldLeft(t2) { case (result, (na, nb)) =>
        substitute(nb, `for` = na, in = result)
      }
    }

    def apply(n: Nat): Nat = {
      ns.foldLeft(n) { case (result, (na, nb)) =>
        substitute(nb, `for` = na, in = result)
      }
    }

    def apply(other: Solution): Solution = {
      Solution(
        ts.mapValues(t => other(t)) ++ other.ts,
        ns.mapValues(n => other(n)) ++ other.ns
      )
    }

    def apply(constraints: Set[Constraint]): Set[Constraint] = {
      constraints.map({
        case TypeConstraint(a, b) =>
          TypeConstraint(apply(a), apply(b))
        case NatConstraint(a, b) =>
          NatConstraint(apply(a), apply(b))
      })
    }
  }

  def solve(cs: Set[Constraint])
           (implicit
            boundT: mutable.Set[DataTypeIdentifier],
            boundN: mutable.Set[NamedVar]): Solution = {
    if (cs.isEmpty) {
      Solution()
    } else {
      def solveAt(pos:Int):Solution = {
        if(pos >= cs.size) error(s"cannot solve constraints")
        val element = cs.toSeq(pos)
        solveOne(element) match {
          case Some(s) => s(solve(s(cs - element)))
          case None => solveAt(pos + 1)
        }
      }
      solveAt(0)
    }
  }

  def solveOne(c: Constraint)
              (implicit
               boundT: mutable.Set[DataTypeIdentifier],
               boundN: mutable.Set[NamedVar]): Option[Solution] = c match {
    case TypeConstraint(a, b) => (a, b) match {
      case (i: DataTypeIdentifier, _) => Some(unifyTypeIdent(i, b))
      case (_, i: DataTypeIdentifier) => Some(unifyTypeIdent(i, a))
      case (_: BasicType, _: BasicType) if a == b =>
        Some(Solution())
      case (IndexType(sa), IndexType(sb)) =>
        solveOne(NatConstraint(sa, sb))
      case (ArrayType(sa, ea), ArrayType(sb, eb)) =>
        Some(solve(Set(NatConstraint(sa, sb), TypeConstraint(ea, eb))))
      case (VectorType(sa, ea), VectorType(sb, eb)) =>
        Some(solve(Set(NatConstraint(sa, sb), TypeConstraint(ea, eb))))
      case (DepArrayType(sa, ea), DepArrayType(sb, eb)) =>
        ???
      case (TupleType(ea@_*), TupleType(eb@_*)) =>
        Some(solve(ea.zip(eb).map({ case (a, b) => TypeConstraint(a, b) }).toSet))
      case (FunctionType(ina, outa), FunctionType(inb, outb)) =>
        Some(solve(Set(TypeConstraint(ina, inb), TypeConstraint(outa, outb))))
      case (DependentFunctionType(na: NatIdentifier, ta), DependentFunctionType(nb: NatIdentifier, tb)) =>
        val n = NamedVar(freshName("n"))
        boundN += n
        boundN -= na
        boundN -= nb
        Some(solve(Set(
          TypeConstraint(substitute(n, `for`=na, in=ta), substitute(n, `for`=nb, in=tb)),
          NatConstraint(n, na), NatConstraint(n, nb)
        )))
      case (DependentFunctionType(dta: DataTypeIdentifier, ta), DependentFunctionType(dtb: DataTypeIdentifier, tb)) =>
        val dt = DataTypeIdentifier(freshName("t"))
        boundT += dt
        boundT -= dta
        boundT -= dtb
        Some(solve(Set(
          TypeConstraint(substitute(dt, `for`=dta, in=ta), substitute(dt, `for`=dtb, in=tb)),
          TypeConstraint(dt, dta), TypeConstraint(dt, dtb)
        )))
      case (NatDataTypeApply(_, _), ArrayType(_, _)) => None

      case _ => error(s"cannot unify $a and $b")
    }
    case NatConstraint(a, b) => Some((a, b) match {
      case (i: NamedVar, _) => nat.unifyIdent(i, b)
      case (_, i: NamedVar) => nat.unifyIdent(i, a)
      case _ if a == b => Solution()
      // case _ if !nat.potentialPivots(a).isEmpty => nat.tryPivots(a, b)
      // case _ if !nat.potentialPivots(b).isEmpty => nat.tryPivots(b, a)
      case (s: lift.arithmetic.Sum, _) => nat.unifySum(s, b)
      case (_, s: lift.arithmetic.Sum) => nat.unifySum(s, a)
      case (p: lift.arithmetic.Prod, _) => nat.unifyProd(p, b)
      case (_, p: lift.arithmetic.Prod) => nat.unifyProd(p, a)
      case _ => error(s"cannot unify $a and $b")
    })
  }

  def unifyTypeIdent(i: DataTypeIdentifier, t: Type)
                    (implicit bound: mutable.Set[DataTypeIdentifier]): Solution = {
    t match {
      case j: DataTypeIdentifier =>
        if (i == j) { Solution() }
        else if (!bound(i)) { Solution.subs(i, j) }
        else if (!bound(j)) { Solution.subs(j, i) }
        else { error(s"cannot unify $i and $j, they are both bound") }
      case _ if occurs(i, t) =>
        error(s"circular use: $i occurs in $t")
      case _ if !bound(i) => Solution.subs(i, t)
      case _ => ???
    }
  }

  private object nat {
    import lift.arithmetic._

    // collect free variables with only 1 occurence
    def potentialPivots(n: Nat)
                       (implicit bound: mutable.Set[NamedVar]): Set[NamedVar] = {
      var free_occurences = mutable.Map[NamedVar, Integer]()
          .withDefault(_ => 0)
      ArithExpr.visit(n, {
        case v: NamedVar if !bound(v) => free_occurences(v) += 1
        case _ =>
      })

      free_occurences.foldLeft(Set[NamedVar]())({ case (potential, (v, c)) =>
          if (c == 1) { potential + v }
          else { potential }
      })
    }

    def pivotSolution(pivot: NamedVar, n: Nat, value: Nat)
                     (implicit bound: mutable.Set[NamedVar]): Option[Solution] = {
      n match {
        case i: NamedVar if i == pivot => Some(Solution.subs(pivot, value))
        case Prod(terms) =>
          val (p, r) = terms.partition(t => ArithExpr.contains(t, pivot))
          if (p.size != 1) {
            None
          } else {
            pivotSolution(pivot, p.head, r.foldLeft(value)({ case (v, r) => v /^ r }))
          }
        case Sum(terms) =>
          val (p, r) = terms.partition(t => ArithExpr.contains(t, pivot))
          if (p.size != 1) {
            None
          } else {
            pivotSolution(pivot, p.head, r.foldLeft(value)({ case (v, r) => v - r }))
          }
        case Pow(b, Cst(-1)) => pivotSolution(pivot, b, Cst(1) /^ value)
        case _ => ???
      }
    }

    def tryPivots(n: Nat, value: Nat)
                 (implicit bound: mutable.Set[NamedVar]): Option[Solution] = {
      val pivots = potentialPivots(n)
      pivots.foreach(pivotSolution(_, n, value) match {
        case Some(s) =>
          return Some(s)
      })
      return None
    }

    def unifyProd(p: Prod, n: Nat)
                 (implicit bound: mutable.Set[NamedVar]): Solution = {
      // n = p --> 1 = p * (1/n)
      tryPivots(p /^ n, 1).get
    }

    def unifySum(s: Sum, n: Nat)
                (implicit bound: mutable.Set[NamedVar]): Solution = {
      // n = s --> 0 = s + (-n)
      tryPivots(s - n, 0).get
    }

    def unifyIdent(i: NamedVar, n: Nat)
                     (implicit bound: mutable.Set[NamedVar]): Solution = {
      n match {
        case j: NamedVar =>
          if (i == j) { Solution() }
          else if (!bound(i)) { Solution.subs(i, j) }
          else if (!bound(j)) { Solution.subs(j, i) }
          else { error(s"cannot unify $i and $j, they are both bound") }
        case _ if !ArithExpr.contains(n, i) && !bound(i) => Solution.subs(i, n)
        case p: Prod => unifyProd(p, i)
        case s: Sum => unifySum(s, i)
        case _ => ???
      }
    }
  }

  def occurs(i: DataTypeIdentifier, t: Type): Boolean = {
    t match {
      case j: DataTypeIdentifier => i == j
      case FunctionType(it, ot) => occurs(i, it) || occurs(i, ot)
      case _ => false
    }
  }
}