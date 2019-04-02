package lift.core.types

import lift.arithmetic.ArithExpr
import lift.core._
import lift.core.lifting._

case class InferenceException(msg: String) extends Exception

object infer {
  def apply(e: Expr): TypedExpr = {
    val constraints: MConstraints = scala.collection.mutable.Set()
    val te = constraint(e, constraints, scala.collection.mutable.Map())
    solve(constraints.toSet)(te)
  }

  def error(msg: String): Nothing =
    throw new InferenceException(msg)

  type Constraints = scala.collection.immutable.Set[Constraint]
  type MConstraints = scala.collection.mutable.Set[Constraint]

  trait Constraint
  case class TypeConstraint(a: Type, b: Type) extends Constraint
  case class NatConstraint(a: Nat, b: Nat) extends Constraint

  def constraint(expr: Expr,
                 constraints: MConstraints,
                 identifierT: scala.collection.mutable.Map[Identifier, Type]
                ): TypedExpr = {
    def fresh(): Type = DataTypeIdentifier(freshName("t"))
    def typed(e: Expr): TypedExpr = constraint(e, constraints, identifierT)

    expr match {
      case i: Identifier =>
        val t = identifierT.get(i) match {
          case None =>
            val newT = fresh()
            identifierT update (i, newT)
            newT
          case Some(oldT) => oldT
        }
        TypedExpr(i, t)

      case Lambda(x, e) =>
        val tx = typed(x)
        val te = typed(e)
        TypedExpr(Lambda(x, te), FunctionType(tx.t, te.t))

      case Apply(f, e) =>
        val tf = typed(f)
        val te = typed(e)
        val ot = fresh()
        constraints += TypeConstraint(tf.t, FunctionType(te.t, ot))
        TypedExpr(Apply(tf, te), ot)

      case NatDepLambda(n, e) =>
        val te = typed(e)
        TypedExpr(NatDepLambda(n, te), NatDependentFunctionType(n, te.t))

      case NatDepApply(f, n) =>
        typed(liftNatDependentFunctionExpr(f).value(n))

      case TypeDepLambda(dt, e) =>
        val te = typed(e)
        TypedExpr(TypeDepLambda(dt, te), TypeDependentFunctionType(dt, te.t))

      case TypeDepApply(f, dt) =>
        typed(liftTypeDependentFunctionExpr(f).value(dt))

      case l: Literal => TypedExpr(l, l.d.dataType)

      case i: Index => TypedExpr(i, IndexType(i.size))

      case n: NatExpr => TypedExpr(n, NatType)

//      case IfThenElse(cond, thenE, elseE) =>
//        val tce = typed(cond)
//        val tte = typed(thenE)
//        val tee = typed(elseE)
//        val ot = fresh()
//        constraints += TypeConstraint(tce.t, bool)
//        constraints += TypeConstraint(tte.t, ot)
//        constraints += TypeConstraint(tee.t, ot)
//        TypedExpr(IfThenElse(tce, tte, tee), ot)

      case TypedExpr(e, t) =>
        val te = typed(e)
        constraints += TypeConstraint(te.t, t)
        TypedExpr(e, t)

      case p: Primitive => TypedExpr(p, p.t)
    }
  }

  object Solution {
    def apply(): Solution = Solution(Map(), Map())
    def subs(ta: Type, tb: Type): Solution = Solution(Map(ta -> tb), Map())
    def subs(na: Nat, nb: Nat): Solution = Solution(Map(), Map(na -> nb))
  }

  case class Solution(ts: Map[Type, Type],
                      ns: Map[Nat, Nat]) {
    def apply(e: TypedExpr): TypedExpr = {
      ???
    }

    def apply(other: Solution): Solution = {
      ???
    }
  }

  def solve(cs: Constraints): Solution = {
    if (cs.isEmpty) { Solution() } else { solveOne(cs.head)(solve(cs.tail)) }
  }

  def solveOne: Constraint => Solution = {
    case TypeConstraint(a, b) => (a, b) match {
      case (i: DataTypeIdentifier, _) => unify(i, b)
      case (_, i: DataTypeIdentifier) => unify(i, a)
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
        solve(Set(NatConstraint(na, nb), TypeConstraint(ta, tb)))
      case (TypeDependentFunctionType(dta, ta), TypeDependentFunctionType(dtb, tb)) =>
        solve(Set(TypeConstraint(dta, dtb), TypeConstraint(ta, tb)))
      case _ => error(s"cannot unify $a and $b")
    }
    case NatConstraint(a, b) => (a, b) match {
      case (i: NatIdentifier, _) => unify(i, b)
      case (_, i: NatIdentifier) => unify(i, a)
      case _ => error(s"cannot unify $a and $b")
    }
  }

  def unify(i: DataTypeIdentifier, t: Type): Solution = {
    t match {
      case _: DataTypeIdentifier =>
        if (i == t) { Solution() } else { Solution.subs(i, t) }
      case _ if occurs(i, t) =>
        error(s"circular use: $i occurs in $t")
      case _ => Solution.subs(i, t)
    }
  }

  def unify(i: NatIdentifier, n: Nat): Solution = {
    n match {
      case _: NatIdentifier =>
        if (i == n) { Solution() } else { Solution.subs(i, n) }
      case _ if ArithExpr.contains(n, i) =>
        error(s"circular use: $i occurs in $n")
      /*
    case Prod()
    case c: Cst
    */
      case _ => Solution.subs(i, n)
    }
  }

  def occurs(i: DataTypeIdentifier, t: Type) = ???
}