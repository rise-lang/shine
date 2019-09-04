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
    // build set of constraints
    val mutableConstraints = mutable.Set[Constraint]()
    val typed_e = constrainTypes(e, mutableConstraints, mutable.Map())
    val constraints = mutableConstraints.toSet
    constraints.foreach(println)

    // solve the constraints
    val (boundT, boundN) = boundIdentifiers(typed_e)
    val solution = solve(constraints)(boundT, boundN)

    // apply the solution
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
  case class NatToDataConstraint(a: NatToData, b: NatToData) extends Constraint {
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
        TypedExpr(Lambda(x, te), FunType(xt, te.t))

      case Apply(f, e) =>
        val tf = typed(f)
        val te = typed(e)
        val ot = fresh()
        val constraint = TypeConstraint(tf.t, FunType(te.t, ot))
        println(s"Constraint for expression `$expr' is `$constraint'")
        constraints += constraint
        TypedExpr(Apply(tf, te), ot)

      case DepLambda(x, e) => x match {
        case n: NatIdentifier =>
          val te = typed(e)
          TypedExpr(DepLambda[NatKind](n, te), DepFunType[NatKind, Type](n, te.t))
        case dt: DataTypeIdentifier =>
          val te = typed(e)
          TypedExpr(DepLambda[DataKind](dt, te), DepFunType[DataKind, Type](dt, te.t))
        case n2n: NatToNatIdentifier =>
          val te = typed(e)
          TypedExpr(DepLambda[NatToNatKind](n2n, te), DepFunType[NatToNatKind, Type](n2n, te.t))
      }

      case DepApply(f, x) => x match {
        case n: Nat =>
          val tf = typed(f)
          TypedExpr(DepApply[NatKind](tf, n), liftDependentFunctionType[NatKind](tf.t)(n))
        case dt: DataType =>
          val tf = typed(f)
          TypedExpr(DepApply[DataKind](tf, dt), liftDependentFunctionType[DataKind](tf.t)(dt))
        case n2n: NatToNat =>
          val tf = typed(f)
          TypedExpr(DepApply[NatToNatKind](tf, n2n), liftDependentFunctionType[NatToNatKind](tf.t)(n2n))
      }

      case l: Literal => TypedExpr(l, l.d.dataType)

      case TypedExpr(e, t) =>
        val te = typed(e)
        val constraint = TypeConstraint(te.t, t)
        println(s"Constraint for expression `$expr' is `$constraint'")
        constraints += constraint
        te

      case p: Primitive => TypedExpr(p, p.t)
    }
  }

  def boundIdentifiers(expr: TypedExpr): (mutable.Set[DataTypeIdentifier], mutable.Set[NamedVar]) = {
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
              case DepFunType(x: NatIdentifier, _) => boundN += x
              case DepFunType(x: DataTypeIdentifier, _) => boundT += x
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
    def apply(): Solution = Solution(Map(), Map(), Map())
    def subs(ta: Type, tb: Type): Solution = Solution(Map(ta -> tb), Map(), Map())
    def subs(na: NamedVar, nb: Nat): Solution = Solution(Map(), Map(na -> nb), Map())
    def subs(na: NatToDataIdentifier, nb: NatToData): Solution = Solution(Map(), Map(), Map(na -> nb))
  }

  case class Solution(ts: Map[Type, Type],
                      ns: Map[NamedVar, Nat],
                      n2ds: Map[NatToDataIdentifier, NatToData]) {
    def apply(e: Expr): Expr = {
      val sol = this
      traversal.DepthFirstLocalResult(e, new traversal.Visitor {
        import traversal.{Result, Continue}
        override def apply(ae: Nat): Result[Nat] = Continue(sol(ae), this)
        override def apply[T <: Type](t: T): Result[T] = Continue(sol(t).asInstanceOf[T], this)
        override def apply(n2d: NatToData): Result[NatToData] = Continue(sol(n2d), this)
      })
    }

    def apply(t: Type): Type = {
      val t2 = ts.foldLeft(t) {
        case (result, (ta, tb)) => substitute(tb, `for` = ta, in = result)
      }
      val t3 = ns.foldLeft(t2) {
        case (result, (na, nb)) => substitute(nb, `for` = na, in = result)
      }
      n2ds.foldLeft(t3) {
        case (result, (na, nb)) => substitute(nb, `for` = na, in = result)
      }
    }

    def apply(n: Nat): Nat = {
      ns.foldLeft(n) {
        case (result, (na, nb)) => substitute(nb, `for` = na, in = result)
      }
    }

    def apply(n2d: NatToData): NatToData = {
      n2ds.foldLeft(n2d) {
        case (result, (na, nb)) => substitute(nb, `for` = na, in = result)
      }
    }

    // concatenating two solutions into a single one
    def ++(other: Solution)
          (implicit
           boundT: mutable.Set[DataTypeIdentifier],
           boundN: mutable.Set[NamedVar]): Solution = {
      // this function combines two solutions by applying all the solutions from s2 to the values in s1
      // it then concatenates the resulting maps
      val combine: (Solution, Solution) => Solution = (s1, s2) => {
        Solution(
          s1.ts.mapValues(t => s2(t)) ++ s2.ts,
          s1.ns.mapValues(n => s2(n)) ++ s2.ns,
          s1.n2ds.mapValues(n => s2(n)) ++ s2.n2ds
        )
      }

      // concatenating two solutions starts by combining them ...
      val s = combine(this, other)
      // ... it then takes all the type values ...
      s.ts.map {
        // ... to look for `NatToDataApply` nodes that should be replaced by `b`,
        // but where the function to call is an identifier ...
        case (NatToDataApply(i: NatToDataIdentifier, n: NamedVar), b) =>
          // ... to find the function, the identifier is used to look up the matching `NatToDataLambda` ...
          s.n2ds.get(i) match {
            case Some(NatToDataLambda(x, body)) =>
              // ... and then the `NatToDataApply` is replaced
              // with the body of the `NatToDataLambda` appropriately substituted ...
              solve( Set( TypeConstraint( substitute(n, `for`=x, in=body), b ) ) )
            case _ => Solution()
          }
        case _ => Solution()
      // ... finally, all resulting solutions are combined into a single one by folding over them
      }.foldLeft(s)(combine)
    }

    def apply(constraints: Set[Constraint]): Set[Constraint] = {
      constraints.map {
        case TypeConstraint(a, b) =>
          TypeConstraint(apply(a), apply(b))
        case NatConstraint(a, b) =>
          NatConstraint(apply(a), apply(b))
        case NatToDataConstraint(a, b) =>
          NatToDataConstraint(apply(a), apply(b))
      }
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
          case Some(s) => s ++ solve(s.apply(cs - element))
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
      case (i: DataTypeIdentifier, _) => Some(dataType.unifyIdent(i, b))
      case (_, i: DataTypeIdentifier) => Some(dataType.unifyIdent(i, a))
      case (_: BasicType, _: BasicType) if a == b =>
        Some(Solution())
      case (IndexType(sa), IndexType(sb)) =>
        solveOne(NatConstraint(sa, sb))
      case (ArrayType(sa, ea), ArrayType(sb, eb)) =>
        Some(solve(Set(NatConstraint(sa, sb), TypeConstraint(ea, eb))))
      case (VectorType(sa, ea), VectorType(sb, eb)) =>
        Some(solve(Set(NatConstraint(sa, sb), TypeConstraint(ea, eb))))
      case (DepArrayType(sa, ea), DepArrayType(sb, eb)) =>
        Some(solve(Set(NatConstraint(sa, sb), NatToDataConstraint(ea, eb))))
      case (TupleType(ea@_*), TupleType(eb@_*)) =>
        Some(solve((ea zip eb).map{ case (aa, bb) => TypeConstraint(aa, bb) }.toSet))
      case (FunType(ina, outa), FunType(inb, outb)) =>
        Some(solve(Set(TypeConstraint(ina, inb), TypeConstraint(outa, outb))))
      case (DepFunType(na: NatIdentifier, ta), DepFunType(nb: NatIdentifier, tb)) =>
        val n = NamedVar(freshName("n"))
        boundN += n
        boundN -= na
        boundN -= nb
        Some(solve(Set(
          TypeConstraint(substitute(n, `for`=na, in=ta), substitute(n, `for`=nb, in=tb)),
          NatConstraint(n, na), NatConstraint(n, nb)
        )))
      case (DepFunType(dta: DataTypeIdentifier, ta), DepFunType(dtb: DataTypeIdentifier, tb)) =>
        val dt = DataTypeIdentifier(freshName("t"))
        boundT += dt
        boundT -= dta
        boundT -= dtb
        Some(solve(Set(
          TypeConstraint(substitute(dt, `for`=dta, in=ta), substitute(dt, `for`=dtb, in=tb)),
          TypeConstraint(dt, dta), TypeConstraint(dt, dtb)
        )))
      case (_: NatToDataApply, dt: DataType) => Some(Solution.subs(a, dt)) // substitute apply by data type
      case (dt: DataType, _: NatToDataApply) => Some(Solution.subs(b, dt)) // substitute apply by data type

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
    case NatToDataConstraint(a, b) => (a, b) match {
      case (i: NatToDataIdentifier, _) => Some(natToData.unifyIdent(i, b))
      case (_, i: NatToDataIdentifier) => Some(natToData.unifyIdent(i, a))
      case _ if a == b => Some(Solution())
      case _ => error(s"cannot unify $a and $b")
    }

  }

  object dataType {
    def unifyIdent(i: DataTypeIdentifier, t: Type)
                  (implicit bound: mutable.Set[DataTypeIdentifier]): Solution = t match {
      case j: DataTypeIdentifier =>
        if (i == j) { Solution() }
        else if (!bound(i)) { Solution.subs(i, j) }
        else if (!bound(j)) { Solution.subs(j, i) }
        else { error(s"cannot unify $i and $j, they are both bound") }
      case _ if occurs(i, t) => error(s"circular use: $i occurs in $t")
      case _ if !bound(i) => Solution.subs(i, t)
      case _ => ???
    }
  }

  private object nat {
    import lift.arithmetic._

    // collect free variables with only 1 occurrence
    def potentialPivots(n: Nat)
                       (implicit bound: mutable.Set[NamedVar]): Set[NamedVar] = {
      val free_occurrences = mutable.Map[NamedVar, Integer]()
        .withDefault(_ => 0)
      ArithExpr.visit(n, {
        case v: NamedVar if !bound(v) => free_occurrences(v) += 1
        case _ =>
      })

      free_occurrences.foldLeft(Set[NamedVar]())({ case (potential, (v, c)) =>
        if (c == 1) { potential + v }
        else { potential }
      })
    }

    def pivotSolution(pivot: NamedVar, n: Nat, value: Nat)
                     (implicit bound: mutable.Set[NamedVar]): Option[Solution] = {
      n match {
        case i: NamedVar if i == pivot => Some(Solution.subs(pivot, value))
        case Prod(terms) =>
          val (p, rest) = terms.partition(t => ArithExpr.contains(t, pivot))
          if (p.size != 1) {
            None
          } else {
            pivotSolution(pivot, p.head, rest.foldLeft(value)({ case (v, r) => v /^ r }))
          }
        case Sum(terms) =>
          val (p, rest) = terms.partition(t => ArithExpr.contains(t, pivot))
          if (p.size != 1) {
            None
          } else {
            pivotSolution(pivot, p.head, rest.foldLeft(value)({ case (v, r) => v - r }))
          }
        case Pow(b, Cst(-1)) => pivotSolution(pivot, b, Cst(1) /^ value)
        case _ => None
      }
    }

    def tryPivots(n: Nat, value: Nat)
                 (implicit bound: mutable.Set[NamedVar]): Option[Solution] = {
      val pivots = potentialPivots(n)
      pivots.foreach(pivotSolution(_, n, value) match {
        case Some(s) => return Some(s)
        case None =>
      })
      //noinspection RemoveRedundantReturn
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
                  (implicit bound: mutable.Set[NamedVar]): Solution = n match {
      case j: NamedVar =>
        if (i == j) { Solution() }
        else if (!bound(i)) { Solution.subs(i, j) }
        else if (!bound(j)) { Solution.subs(j, i) }
        else { error(s"cannot unify $i and $j, they are both bound") }
      case _ if !ArithExpr.contains(n, i) && !bound(i) => Solution.subs(i, n)
      case p: Prod => unifyProd(p, i)
      case s: Sum => unifySum(s, i)
      case _ => error(s"cannot unify $i and $n")
    }
  }

  object natToData {
    def unifyIdent(i: NatToDataIdentifier, n: NatToData): Solution = n match {
      case j: NatToDataIdentifier =>
        if (i == j) { Solution() }
        else { error(s"cannot unify $i and $j, they are both bound") }
      case _ => Solution.subs(i, n)
    }
  }

  def occurs(i: DataTypeIdentifier, t: Type): Boolean = t match {
    case j: DataTypeIdentifier => i == j
    case FunType(it, ot) => occurs(i, it) || occurs(i, ot)
    case _ => false
  }
}