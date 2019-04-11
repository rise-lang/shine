package lift.core.types

import lift.core._
import lift.core.lifting._

case class InferenceException(msg: String) extends Exception {
  override def toString = s"inference exception: $msg"
}

object infer {
  def apply(e: Expr): TypedExpr = {
    val constraints: MConstraints = scala.collection.mutable.Set()
    val typed_e = constrainTypes(e, constraints, scala.collection.mutable.Map())
    implicit val (boundT, boundN) = boundIdentifiers(typed_e)
    val solution = solve(constraints.toSet)
    solution(typed_e).asInstanceOf[TypedExpr]
  }

  def error(msg: String): Nothing =
    throw InferenceException(msg)

  type Constraints = scala.collection.immutable.Set[Constraint]
  type MConstraints = scala.collection.mutable.Set[Constraint]

  trait Constraint
  case class TypeConstraint(a: Type, b: Type) extends Constraint
  case class NatConstraint(a: Nat, b: Nat) extends Constraint

  def constrainTypes(expr: Expr,
                     constraints: MConstraints,
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
  : (Set[DataTypeIdentifier], Set[NatIdentifier]) = {
    val boundT = scala.collection.mutable.Set[DataTypeIdentifier]()
    val boundN = scala.collection.mutable.Set[NatIdentifier]()

    case class Visitor() extends traversal.Visitor {
      override def apply(e: Expr): traversal.Result[Expr] = {
        e match {
          case NatLambda(x, _) => boundN += x
          case TypeLambda(x, _) => boundT += x
          case _ =>
        }
        traversal.Continue(e, this)
      }

      override def apply[T <: Type](t: T): T = {
        visitT(t)
        t
      }
    }

    def visitT: Type => Unit = {
      case NatDependentFunctionType(x, t) =>
        boundN += x
        visitT(t)
      case TypeDependentFunctionType(x, t) =>
        boundT += x
        visitT(t)
      case FunctionType(it, ot) =>
        visitT(it)
        visitT(ot)
      case _ =>
    }

    traversal.DepthFirstLocalResult(expr, Visitor())
    (boundT.toSet, boundN.toSet)
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
        override def apply(ae: Nat): Nat = sol(ae)
        override def apply[T <: Type](t: T): T = sol(t).asInstanceOf[T]
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

    def apply(constraints: Constraints): Constraints = {
      constraints.map({
        case TypeConstraint(a, b) =>
          TypeConstraint(apply(a), apply(b))
        case NatConstraint(a, b) =>
          NatConstraint(apply(a), apply(b))
      })
    }
  }

  def solve(cs: Constraints)
           (implicit
            boundT: Set[DataTypeIdentifier],
            boundN: Set[NatIdentifier]): Solution = {
    if (cs.isEmpty) {
      Solution()
    } else {
      val s = solveOne(cs.head)
      s(solve(s(cs.tail)))
    }
  }

  def solveOne(c: Constraint)
              (implicit
               boundT: Set[DataTypeIdentifier],
               boundN: Set[NatIdentifier]): Solution = c match {
    case TypeConstraint(a, b) => (a, b) match {
      case (i: DataTypeIdentifier, _) => unifyTypeIdent(i, b)
      case (_, i: DataTypeIdentifier) => unifyTypeIdent(i, a)
      case (_: BasicType, _: BasicType) if a == b =>
        Solution()
      case (IndexType(sa), IndexType(sb)) =>
        solve(Set(NatConstraint(sa, sb)))
      case (ArrayType(sa, ea), ArrayType(sb, eb)) =>
        solve(Set(NatConstraint(sa, sb), TypeConstraint(ea, eb)))
      case (DepArrayType(sa, ea), DepArrayType(sb, eb)) =>
        ???
      case (TupleType(ea @ _*), TupleType(eb @ _*)) =>
        solve(ea.zip(eb).map({ case (a, b) => TypeConstraint(a, b) }).toSet)
      case (FunctionType(ina, outa), FunctionType(inb, outb)) =>
        solve(Set(TypeConstraint(ina, inb), TypeConstraint(outa, outb)))
      case (NatDependentFunctionType(na, ta), NatDependentFunctionType(nb, tb)) =>
        solve(Set(TypeConstraint(ta, substitute(na, `for`=nb, in=tb))))
      case (TypeDependentFunctionType(dta, ta), TypeDependentFunctionType(dtb, tb)) =>
        solve(Set(TypeConstraint(ta, substitute(dta, `for`=dtb, in=tb))))
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
                    (implicit bound: Set[DataTypeIdentifier]): Solution = {
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
                      (implicit bound: Set[NatIdentifier]): Option[NatIdentifier] = {
      term match {
        case i: NatIdentifier if !bound(i) => Some(i)
        case Prod(Cst(_) :: t :: Nil) => unwrapFreeTerm(t)
        case Sum(Cst(_) :: t :: Nil) => unwrapFreeTerm(t)
        case Pow(b, Cst(_)) => unwrapFreeTerm(b)
        case _ => None
      }
    }

    def findPivot(terms: Seq[ArithExpr])
                 (implicit bound: Set[NatIdentifier]): Nat = {
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
                 (implicit bound: Set[NatIdentifier]): Solution = {
      val Prod(terms) = p
      // n = pivot * .. --> pivot = n / ..
      val pivot = findPivot(terms)
      val value = terms.filter({_ != pivot }).map(n => Cst(1) /^ n).
        foldLeft(n)({ case (x, t) => x * t })
      pivotSolution(pivot, value)
    }

    def unifySum(s: Sum, n: Nat)
                (implicit bound: Set[NatIdentifier]): Solution = {
      val Sum(terms) = s
      // i = pivot + .. --> pivot = i - ..
      val pivot = findPivot(terms)
      val value = terms.filter({ _ != pivot }).map(n => -1 * n).
        foldLeft(n)({ case (x, t) => x + t })
      pivotSolution(pivot, value)
    }

    def unifyIdent(i: NatIdentifier, n: Nat)
                     (implicit bound: Set[NatIdentifier]): Solution = {
      n match {
        case j: NatIdentifier =>
          if (i == j) { Solution() }
          else if (!bound(i)) { Solution.subs(i, j) }
          else if (!bound(j)) { Solution.subs(j, i) }
          else { ??? }
        case _ if ArithExpr.contains(n, i) =>
          error(s"circular use: $i occurs in $n")
        case _ if !bound(i) => Solution.subs(i, n)
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