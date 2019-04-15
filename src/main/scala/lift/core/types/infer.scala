package lift.core.types

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
    val solution = solve(constraints.toSet)
    val result = solution(typed_e)
    if (!isClosedForm(result)) {
      error("expression is not in closed form after inference")
    }
    result.asInstanceOf[TypedExpr]
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
    def freshNat(): NatIdentifier = lift.arithmetic.NamedVar(freshName("_n"))
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

      case NatLambda(n, e) =>
        val te = typed(e)
        TypedExpr(NatLambda(n, te), NatDependentFunctionType(n, te.t))

      case NatApply(f, n) =>
        val tf = typed(f)
        TypedExpr(NatApply(tf, n), liftNatDependentFunctionType(tf.t)(n))

      case TypeLambda(dt, e) =>
        val te = typed(e)
        TypedExpr(TypeLambda(dt, te), TypeDependentFunctionType(dt, te.t))

      case TypeApply(f, dt) =>
        val tf = typed(f)
        TypedExpr(TypeApply(tf, dt), liftTypeDependentFunctionType(tf.t)(dt))

      case l: Literal => TypedExpr(l, l.d.dataType)

      case i: Index => TypedExpr(i, IndexType(i.size))

      case n: NatExpr => TypedExpr(n, NatType)

      case IfThenElse(cond, thenE, elseE) =>
        val tce = typed(cond)
        val tte = typed(thenE)
        val tee = typed(elseE)
        val ot = fresh()
        constraints += TypeConstraint(tce.t, bool)
        constraints += TypeConstraint(tte.t, ot)
        constraints += TypeConstraint(tee.t, ot)
        TypedExpr(IfThenElse(tce, tte, tee), ot)

      case TypedExpr(e, t) =>
        val te = typed(e)
        constraints += TypeConstraint(te.t, t)
        te

      case p: Primitive => TypedExpr(p, p.t)
    }
  }

  def boundIdentifiers(expr: TypedExpr)
  : (mutable.Set[DataTypeIdentifier], mutable.Set[NatIdentifier]) = {
    import traversal.{Result, Continue}

    val boundT = mutable.Set[DataTypeIdentifier]()
    val boundN = mutable.Set[NatIdentifier]()

    case class Visitor() extends traversal.Visitor {
      override def apply(e: Expr): Result[Expr] = {
        e match {
          case NatLambda(x, _) => boundN += x
          case TypeLambda(x, _) => boundT += x
          case _ =>
        }
        Continue(e, this)
      }

      override def apply[T <: Type](t: T): Result[T] = {
        val r = traversal.types.DepthFirstLocalResult(t, new traversal.Visitor() {
          override def apply[T <: Type](t: T): Result[T] = {
            t match {
              case NatDependentFunctionType(x, _) => boundN += x
              case TypeDependentFunctionType(x, _) => boundT += x
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

  def isClosedForm(expr: Expr): Boolean = {
    import traversal.{Result, Continue, Stop}

    case class Visitor(boundV: Set[Identifier],
                       boundT: Set[DataTypeIdentifier],
                       boundN: Set[NatIdentifier]) extends traversal.Visitor {
      override def apply(e: Expr): Result[Expr] = {
        e match {
          case i: Identifier if !boundV(i) => Stop(i)
          case Lambda(x, _) => Continue(e, this.copy(boundV = boundV + x))
          case NatLambda(x, _) => Continue(e, this.copy(boundN = boundN + x))
          case TypeLambda(x, _) => Continue(e, this.copy(boundT = boundT + x))
          case _ => Continue(e, this)
        }
      }

      override def apply(ae: Nat): Result[Nat] = visitNat(ae, boundN, this)

      override def apply[T <: Type](t: T): Result[T] = {
        case class TypeVisitor(boundT: Set[DataTypeIdentifier],
                               boundN: Set[NatIdentifier]) extends traversal.Visitor {
          override def apply[T <: Type](t: T): Result[T] = {
            t match {
              case i: DataTypeIdentifier if !boundT(i) => Stop(t)
              case NatDependentFunctionType(x, _) => Continue(t, this.copy(boundN = boundN + x))
              case TypeDependentFunctionType(x, _) => Continue(t, this.copy(boundT = boundT + x))
              case _ => Continue(t, this)
            }
          }

          override def apply(ae: Nat): Result[Nat] = visitNat(ae, boundN, this)
        }
        Continue(traversal.types.DepthFirstLocalResult(t, TypeVisitor(boundT, boundN)), this)
      }
    }

    def visitNat(ae: Nat, bound: Set[NatIdentifier], v: traversal.Visitor): Result[Nat] = {
      val closed = ae.varList.foldLeft(true)({
        case (c, v: NatIdentifier) => c && bound(v)
        case (c, _) => c
      })
      if (closed) { Continue(ae, v) } else { Stop(ae) }
    }

    traversal.DepthFirstGlobalResult(expr, Visitor(Set(), Set(), Set())) match {
      case Stop(_) => false
      case Continue(_, _) => true
    }
  }

  object Solution {
    def apply(): Solution = Solution(Map(), Map())
    def subs(ta: Type, tb: Type): Solution = Solution(Map(ta -> tb), Map())
    def subs(na: NatIdentifier, nb: Nat): Solution = Solution(Map(), Map(na -> nb))
  }

  case class Solution(ts: Map[Type, Type],
                      ns: Map[NatIdentifier, Nat]) {
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
            boundN: mutable.Set[NatIdentifier]): Solution = {
    if (cs.isEmpty) {
      Solution()
    } else {
      val s = solveOne(cs.head)
      s(solve(s(cs.tail)))
    }
  }

  def solveOne(c: Constraint)
              (implicit
               boundT: mutable.Set[DataTypeIdentifier],
               boundN: mutable.Set[NatIdentifier]): Solution = c match {
    case TypeConstraint(a, b) => (a, b) match {
      case (i: DataTypeIdentifier, _) => unifyTypeIdent(i, b)
      case (_, i: DataTypeIdentifier) => unifyTypeIdent(i, a)
      case (_: BasicType, _: BasicType) if a == b =>
        Solution()
      case (IndexType(sa), IndexType(sb)) =>
        solveOne(NatConstraint(sa, sb))
      case (ArrayType(sa, ea), ArrayType(sb, eb)) =>
        solve(Set(NatConstraint(sa, sb), TypeConstraint(ea, eb)))
      case (VectorType(sa, ea), VectorType(sb, eb)) =>
        solve(Set(NatConstraint(sa, sb), TypeConstraint(ea, eb)))
      case (DepArrayType(sa, ea), DepArrayType(sb, eb)) =>
        ???
      case (TupleType(ea@_*), TupleType(eb@_*)) =>
        solve(ea.zip(eb).map({ case (a, b) => TypeConstraint(a, b) }).toSet)
      case (FunctionType(ina, outa), FunctionType(inb, outb)) =>
        solve(Set(TypeConstraint(ina, inb), TypeConstraint(outa, outb)))
      case (NatDependentFunctionType(na, ta), NatDependentFunctionType(nb, tb)) =>
        val n = lift.arithmetic.NamedVar(freshName("n"))
        boundN += n
        boundN -= na
        boundN -= nb
        solve(Set(
          TypeConstraint(substitute(n, `for`=na, in=ta), substitute(n, `for`=nb, in=tb)),
          NatConstraint(n, na), NatConstraint(n, nb)
        ))
      case (TypeDependentFunctionType(dta, ta), TypeDependentFunctionType(dtb, tb)) =>
        val dt = DataTypeIdentifier(freshName("t"))
        boundT += dt
        boundT -= dta
        boundT -= dtb
        solve(Set(
          TypeConstraint(substitute(dt, `for`=dta, in=ta), substitute(dt, `for`=dtb, in=tb)),
          TypeConstraint(dt, dta), TypeConstraint(dt, dtb)
        ))
      case _ => error(s"cannot unify $a and $b")
    }
    case NatConstraint(a, b) => (a, b) match {
      case (i: NatIdentifier, _) => nat.unifyIdent(i, b)
      case (_, i: NatIdentifier) => nat.unifyIdent(i, a)
      case _ if a == b => Solution()
      case _ if nat.unwrapFreeTerm(a).isDefined => nat.pivotSolution(a, b)
      case _ if nat.unwrapFreeTerm(b).isDefined => nat.pivotSolution(b, a)
      case (s: lift.arithmetic.Sum, _) => nat.unifySum(s, b)
      case (_, s: lift.arithmetic.Sum) => nat.unifySum(s, a)
      case (p: lift.arithmetic.Prod, _) => nat.unifyProd(p, b)
      case (_, p: lift.arithmetic.Prod) => nat.unifyProd(p, a)
      case _ => error(s"cannot unify $a and $b")
    }
  }

  def unifyTypeIdent(i: DataTypeIdentifier, t: Type)
                    (implicit bound: mutable.Set[DataTypeIdentifier]): Solution = {
    t match {
      case j: DataTypeIdentifier =>
        if (i == j) { Solution() }
        else if (!bound(i)) { Solution.subs(i, j) }
        else if (!bound(j)) { Solution.subs(j, i) }
        else { ??? }
      case _ if occurs(i, t) =>
        error(s"circular use: $i occurs in $t")
      case _ if !bound(i) => Solution.subs(i, t)
      case _ => ???
    }
  }

  object nat {
    import lift.arithmetic._

    def unwrapFreeTerm(term: ArithExpr)
                      (implicit bound: mutable.Set[NatIdentifier]): Option[NatIdentifier] = {
      term match {
        case i: NatIdentifier if !bound(i) => Some(i)
        case Prod(Cst(_) :: t :: Nil) => unwrapFreeTerm(t)
        case Sum(Cst(_) :: t :: Nil) => unwrapFreeTerm(t)
        case Pow(b, Cst(_)) => unwrapFreeTerm(b)
        case _ => None
      }
    }

    def findPivot(terms: Seq[ArithExpr])
                 (implicit bound: mutable.Set[NatIdentifier]): Nat = {
      val (free, toTerm) =
        terms.foldLeft((Set[NatIdentifier](), Map[NatIdentifier, Nat]()))(
          { case ((fr, tt), term) =>
            unwrapFreeTerm(term) match {
              case Some(x) => (fr + x, tt updated (x, term))
              case None => (fr, tt)
            }
          })
      val pivots = free.filter(p => terms.count(ArithExpr.contains(_, p)) == 1)
      if (pivots.isEmpty) { ??? }
      toTerm(pivots.head)
    }

    def pivotSolution(term: Nat, value: Nat): Solution = {
      term match {
        case i: NatIdentifier => Solution.subs(i, value)
        case Prod((c: Cst) :: t :: Nil) => pivotSolution(t, value /^ c)
        case Sum((c: Cst) :: t :: Nil) => pivotSolution(t, value - c)
        case Pow(b, Cst(-1)) => pivotSolution(b, Cst(1) /^ value)
        case _ => ???
      }
    }

    def unifyProd(p: Prod, n: Nat)
                 (implicit bound: mutable.Set[NatIdentifier]): Solution = {
      val Prod(ps) = p
      // n = ps --> 1 = ss * (1/n)
      val terms = (Cst(1) /^ n) :: ps
      // 1 = pivot * .. --> pivot = 1 / ..
      val pivot = findPivot(terms)
      val value = terms.filter({_ != pivot }).foldLeft(1: Nat)({ case (x, t) => x /^ t })
      pivotSolution(pivot, value)
    }

    def unifySum(s: Sum, n: Nat)
                (implicit bound: mutable.Set[NatIdentifier]): Solution = {
      val Sum(ss) = s
      // n = ss --> 0 = ss + (-n)
      val terms = (-1*n) :: ss
      // 0 = pivot + .. --> pivot = 0 - ..
      val pivot = findPivot(terms)
      val value = terms.filter({ _ != pivot }).foldLeft(0: Nat)({ case (x, t) => x - t })
      pivotSolution(pivot, value)
    }

    def unifyIdent(i: NatIdentifier, n: Nat)
                     (implicit bound: mutable.Set[NatIdentifier]): Solution = {
      n match {
        case j: NatIdentifier =>
          if (i == j) { Solution() }
          else if (!bound(i)) { Solution.subs(i, j) }
          else if (!bound(j)) { Solution.subs(j, i) }
          else { ??? }
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