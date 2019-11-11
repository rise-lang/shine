package lift.core.types

import lift.arithmetic.NamedVar
import lift.core._
import lift.core.lifting._
import lift.core.DSL._

import scala.collection.mutable

case class InferenceException(msg: String) extends Exception {
  override def toString = s"inference exception: $msg"
}

object infer {
  def apply(e: Expr): Expr = {
    // build set of constraints
    val constraintList = mutable.ArrayBuffer[Constraint]()
    val typed_e = constrainTypes(e, constraintList, mutable.Map())
    val constraints = constraintList.toSeq
    //constraints.foreach(println)

    // solve the constraints
    val bound = boundIdentifiers(typed_e)
    val solution = solve(constraints)(bound)

    // apply the solution
    val r = solution(typed_e)
    printTypesAndTypeHoles(r)
    r
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
  case class AddressSpaceConstraint(a: AddressSpace, b: AddressSpace) extends Constraint {
    override def toString: String = s"$a  ~  $b"
  }
  case class NatToDataConstraint(a: NatToData, b: NatToData) extends Constraint {
    override def toString: String = s"$a  ~  $b"
  }
  case class DepConstraint[K <: Kind](a: Type, x: K#T, b: Type) extends Constraint {
    override def toString: String = s"$a ($x) ~ $b"
  }

  def constrainTypes(expr: Expr,
                     constraints: mutable.ArrayBuffer[Constraint],
                     env: mutable.Map[String, Type]
                    ): Expr = {
    def typed(e: Expr): Expr = constrainTypes(e, constraints, env)
    def genType(e: Expr): Type = if (e.t == TypePlaceholder) freshTypeIdentifier else e.t

    expr match {
      case i: Identifier =>
        val t = env
          .getOrElse(i.name, error(s"$i has no type in the environment"))
        i.setType(t)

      case Lambda(x, e) =>
        val tx = x.setType(genType(x))
        env update (tx.name, tx.t)
        val te = typed(e)
        env remove tx.name
        val ft = FunType(tx.t, te.t)
        val exprT = genType(expr)
        val constraint = TypeConstraint(exprT, ft)
        constraints += constraint
        Lambda(tx, te)(ft)

      case App(f, e) =>
        val tf = typed(f)
        val te = typed(e)
        val exprT = genType(expr)
        val constraint = TypeConstraint(tf.t, FunType(te.t, exprT))
        constraints += constraint
        App(tf, te)(exprT)

      case DepLambda(x, e) =>
        val te = typed(e)
        val exprT = genType(expr)
        val tf = x match {
          case n: NatIdentifier =>
            DepLambda[NatKind](n, te)(DepFunType[NatKind, Type](n, te.t))
          case dt: DataTypeIdentifier =>
            DepLambda[DataKind](dt, te)(DepFunType[DataKind, Type](dt, te.t))
          case ad: AddressSpaceIdentifier =>
            DepLambda[AddressSpaceKind](ad, te)(DepFunType[AddressSpaceKind, Type](ad, te.t))
          case n2n: NatToNatIdentifier =>
            DepLambda[NatToNatKind](n2n, te)(DepFunType[NatToNatKind, Type](n2n, te.t))
        }
        val constraint = TypeConstraint(exprT, tf.t)
        constraints += constraint
        tf

      case DepApp(f, x) =>
        val tf = typed(f)
        val exprT = genType(expr)
        val constraint = DepConstraint(tf.t, x, exprT)
        constraints += constraint
        DepApp(tf, x)(exprT)

      case l: Literal => l

      case p: Primitive => p.setType(p.typeScheme)
    }
  }

  def boundIdentifiers(expr: Expr): mutable.Set[Kind.Identifier] = {
    import traversal.{Result, Continue}

    val bound = mutable.Set[Kind.Identifier]()

    case class Visitor() extends traversal.Visitor {
      override def visitExpr(e: Expr): Result[Expr] = {
        e match {
          case DepLambda(x, _) => bound += x
          case _ =>
        }
        Continue(e, this)
      }

      override def visitType[T <: Type](t: T): Result[T] = {
        val r = traversal.types.DepthFirstLocalResult(t, new traversal.Visitor() {
          override def visitType[U <: Type](u: U): Result[U] = {
            u match {
              case DepFunType(x, _) => bound += x
              case _ =>
            }
            Continue(u, this)
          }
        })
        Continue(r, this)
      }
    }

    traversal.DepthFirstLocalResult(expr, Visitor())
    bound
  }

  object Solution {
    def apply(): Solution = Solution(Map(), Map(), Map(), Map())
    def subs(ta: Type, tb: Type): Solution = Solution(Map(ta -> tb), Map(), Map(), Map())
    def subs(ta: DataTypeIdentifier, tb: Type): Solution = Solution(Map(ta -> tb), Map(), Map(), Map())
    def subs(na: NamedVar, nb: Nat): Solution = Solution(Map(), Map(na -> nb), Map(), Map())
    def subs(aa: AddressSpaceIdentifier, ab: AddressSpace): Solution = Solution(Map(), Map(), Map(aa -> ab), Map())
    def subs(na: NatToDataIdentifier, nb: NatToData): Solution = Solution(Map(), Map(), Map(), Map(na -> nb))
  }

  case class Solution(ts: Map[Type, Type],
                      ns: Map[NamedVar, Nat],
                      as: Map[AddressSpaceIdentifier, AddressSpace],
                      n2ds: Map[NatToDataIdentifier, NatToData]) {
    import traversal.{Result, Stop, Continue}

    case class Visitor(sol: Solution) extends traversal.Visitor {
      override def visitNat(ae: Nat): Result[Nat] = Stop(sol(ae))
      override def visitType[T <: Type](t: T): Result[T] = Stop(sol(t).asInstanceOf[T])
      override def visitAddressSpace(a: AddressSpace): Result[AddressSpace] = Stop(sol(a))
      override def visitN2D(n2d: NatToData): Result[NatToData] = Stop(sol(n2d))
    }

    def apply(e: Expr): Expr = {
      traversal.DepthFirstLocalResult(e, Visitor(this))
    }

    def apply(t: Type): Type = {
      traversal.types.DepthFirstLocalResult(t, new Visitor(this) {
        override def visitType[T <: Type](t: T): Result[T] = sol.ts.get(t) match {
          case Some(x) => Stop(x.asInstanceOf[T])
          case None => Continue(t, this)
        }
      })
    }

    def apply(n: Nat): Nat = {
      ns.foldLeft(n) {
        case (result, (na, nb)) => substitute.natInNat(nb, `for` = na, in = result)
      }
    }

    def apply(a: AddressSpace): AddressSpace = {
      as.foldLeft(a) {
        case (result, (aa, ab)) => substitute.addressSpaceInAddressSpace(ab, `for` = aa, in = result)
      }
    }

    def apply(n2d: NatToData): NatToData = {
      n2ds.foldLeft(n2d) {
        case (result, (na, nb)) => substitute.n2dInN2d(nb, `for` = na, in = result)
      }
    }

    // concatenating two solutions into a single one
    def ++(other: Solution)
          (implicit bound: mutable.Set[Kind.Identifier]): Solution = {
      // this function combines two solutions by applying all the solutions from s2 to the values in s1
      // it then concatenates the resulting maps
      val combine: (Solution, Solution) => Solution = (s1, s2) => {
        Solution(
          s1.ts.mapValues(t => s2(t)) ++ s2.ts,
          s1.ns.mapValues(n => s2(n)) ++ s2.ns,
          s1.as.mapValues(a => s2(a)) ++ s2.as,
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
              solve(Seq(TypeConstraint(substitute.natInDataType(n, `for`=x, in=body), b)))
            case _ => Solution()
          }
        case _ => Solution()
      // ... finally, all resulting solutions are combined into a single one by folding over them
      }.foldLeft(s)(combine)
    }

    def apply(constraints: Seq[Constraint]): Seq[Constraint] = {
      constraints.map {
        case TypeConstraint(a, b) =>
          TypeConstraint(apply(a), apply(b))
        case AddressSpaceConstraint(a, b) =>
          AddressSpaceConstraint(apply(a), apply(b))
        case NatConstraint(a, b) =>
          NatConstraint(apply(a), apply(b))
        case NatToDataConstraint(a, b) =>
          NatToDataConstraint(apply(a), apply(b))
        case DepConstraint(a, x: Nat, b) =>
          DepConstraint[NatKind](apply(a), apply(x), apply(b))
        case DepConstraint(a, x: DataType, b) =>
          DepConstraint[DataKind](apply(a), apply(x).asInstanceOf[DataType], apply(b))
        case DepConstraint(a, x: AddressSpace, b) =>
          DepConstraint[AddressSpaceKind](apply(a), apply(x), apply(b))
      }
    }
  }

  def solve(cs: Seq[Constraint])
           (implicit bound: mutable.Set[Kind.Identifier]): Solution = cs match {
    case Nil => Solution()
    case c +: cs => solveOne(c) match {
      case Some(s) => s ++ solve(s.apply(cs))
      case None => error(s"cannot solve $c")
    }
  }

  @scala.annotation.tailrec
  def solveOne(c: Constraint)
              (implicit bound: mutable.Set[Kind.Identifier]): Option[Solution] = c match {
    case TypeConstraint(a, b) => (a, b) match {
      case (i: TypeIdentifier, _) => Some(unifyTypeIdent(i, b))
      case (_, i: TypeIdentifier) => Some(unifyTypeIdent(i, a))
      case (i: DataTypeIdentifier, dt: DataType) => Some(unifyDataTypeIdent(i, dt))
      case (dt: DataType, i: DataTypeIdentifier) => Some(unifyDataTypeIdent(i, dt))
      case (b1: BasicType, b2: BasicType) if b1 == b2 =>
        Some(Solution())
      case (IndexType(sa), IndexType(sb)) =>
        solveOne(NatConstraint(sa, sb))
      case (ArrayType(sa, ea), ArrayType(sb, eb)) =>
        Some(solve(Seq(NatConstraint(sa, sb), TypeConstraint(ea, eb))))
      case (VectorType(sa, ea), VectorType(sb, eb)) =>
        Some(solve(Seq(NatConstraint(sa, sb), TypeConstraint(ea, eb))))
      case (DepArrayType(sa, ea), DepArrayType(sb, eb)) =>
        Some(solve(Seq(NatConstraint(sa, sb), NatToDataConstraint(ea, eb))))
      case (PairType(pa1, pa2), PairType(pb1, pb2)) =>
        Some(solve(Seq(TypeConstraint(pa1, pb1), TypeConstraint(pa2, pb2))))
      case (FunType(ina, outa), FunType(inb, outb)) =>
        Some(solve(Seq(TypeConstraint(ina, inb), TypeConstraint(outa, outb))))
      case (DepFunType(na: NatIdentifier, ta), DepFunType(nb: NatIdentifier, tb)) =>
        val n = NatIdentifier(freshName("n"))
        bound += n
        bound -= na
        bound -= nb
        Some(solve(Seq(NatConstraint(n, na), NatConstraint(n, nb),
          TypeConstraint(ta, tb))))
      case (DepFunType(dta: DataTypeIdentifier, ta), DepFunType(dtb: DataTypeIdentifier, tb)) =>
        val dt = DataTypeIdentifier(freshName("t"))
        bound += dt
        bound -= dta
        bound -= dtb
        Some(solve(Seq(TypeConstraint(dt, dta), TypeConstraint(dt, dtb),
          TypeConstraint(ta, tb))))
      case (DepFunType(asa: AddressSpaceIdentifier, ta), DepFunType(asb: AddressSpaceIdentifier, tb)) => ???
      case (_: NatToDataApply, dt: DataType) => Some(Solution.subs(a, dt)) // substitute apply by data type
      case (dt: DataType, _: NatToDataApply) => Some(Solution.subs(b, dt)) // substitute apply by data type
      case _ => error(s"cannot unify $a and $b")
    }

    case DepConstraint(a, x, b) => Some(a match {
      case _: DepFunType[_, _] => solve(Seq(TypeConstraint(liftDependentFunctionType(a)(x), b)))
      case _ => error(s"expected a dependent function type, but got $a")
    })

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
      case (p: lift.arithmetic.Pow, _) => nat.unifyProd(p, b)
      case (_, p: lift.arithmetic.Pow) => nat.unifyProd(p, a)
      case (p: lift.arithmetic.IntDiv, _) => nat.unifyProd(p, b)
      case (_, p: lift.arithmetic.IntDiv) => nat.unifyProd(p, a)
      case _ => error(s"cannot unify $a and $b")
    })

    case NatToDataConstraint(a, b) => (a, b) match {
      case (i: NatToDataIdentifier, _) => Some(natToData.unifyIdent(i, b))
      case (_, i: NatToDataIdentifier) => Some(natToData.unifyIdent(i, a))
      case _ if a == b => Some(Solution())
      case _ => error(s"cannot unify $a and $b")
    }

  }

  def unifyTypeIdent(i: TypeIdentifier, t: Type)
                    (implicit bound: mutable.Set[Kind.Identifier]): Solution = {
    Solution.subs(i, t)
  }

  // FIXME: datatypes and types are mixed up
  def unifyDataTypeIdent(i: DataTypeIdentifier, t: DataType)
                        (implicit bound: mutable.Set[Kind.Identifier]): Solution = {
    t match {
      case j: DataTypeIdentifier =>
        if (i == j) { Solution() }
        else if (!bound(i)) { Solution.subs(i, j) }
        else if (!bound(j)) { Solution.subs(j, i) }
        else { error(s"cannot unify $i and $j, they are both bound") }
      case _ if occurs(i, t) => error(s"circular use: $i occurs in $t")
      case _ if !bound(i) => Solution.subs(i, t)
    }
  }

  private object nat {
    import lift.arithmetic._

    def freeOccurences(n: Nat)
                      (implicit bound: mutable.Set[Kind.Identifier]): Map[NamedVar, Integer] = {
      val free_occurrences = mutable.Map[NamedVar, Integer]()
        .withDefault(_ => 0)
      ArithExpr.visit(n, {
        case v: NamedVar if !bound(NatIdentifier(v)) => free_occurrences(v) += 1
        case _ =>
      })
      free_occurrences.toMap
    }

    // collect free variables with only 1 occurrence
    def potentialPivots(n: Nat)
                       (implicit bound: mutable.Set[Kind.Identifier]): Set[NamedVar] = {
      freeOccurences(n).foldLeft(Set[NamedVar]())({ case (potential, (v, c)) =>
        if (c == 1) { potential + v }
        else { potential }
      })
    }

    @scala.annotation.tailrec
    def pivotSolution(pivot: NamedVar, n: Nat, value: Nat)
                     (implicit bound: mutable.Set[Kind.Identifier]): Option[Solution] = {
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
                 (implicit bound: mutable.Set[Kind.Identifier]): Solution = {
      potentialPivots(n).foreach(pivotSolution(_, n, value) match {
        case Some(s) => return s
        case None =>
      })
      error(s"could not pivot $n = $value")
    }

    def unifyProd(p: Nat, n: Nat)
                 (implicit bound: mutable.Set[Kind.Identifier]): Solution = {
      // n = p --> 1 = p * (1/n)
      tryPivots(p /^ n, 1)
    }

    def unifySum(s: Sum, n: Nat)
                (implicit bound: mutable.Set[Kind.Identifier]): Solution = {
      // n = s --> 0 = s + (-n)
      tryPivots(s - n, 0)
    }

    def unifyIdent(i: NamedVar, n: Nat)
                  (implicit bound: mutable.Set[Kind.Identifier]): Solution = n match {
      case j: NamedVar =>
        if (i == j) { Solution() }
        else if (!bound(NatIdentifier(i))) { Solution.subs(i, j) }
        else if (!bound(NatIdentifier(j))) { Solution.subs(j, i) }
        else { error(s"cannot unify $i and $j, they are both bound") }
      case _ if !ArithExpr.contains(n, i) && !bound(NatIdentifier(i)) => Solution.subs(i, n)
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
    case FunType(it, ot) => occurs(i, it) || occurs(i, ot)
    case _ => false
  }

  def printTypesAndTypeHoles(expr: Expr): Unit = {
    var holeFound = false
    traversal.DepthFirstLocalResult(expr, new traversal.Visitor {
      override def visitExpr(e: Expr): traversal.Result[Expr] = {
        e match {
          case h: primitives.TypeHole =>
            println(s"found type hole ${h.msg}: ${h.t}")
            holeFound = true
          case p: primitives.PrintType =>
            println(s"${p.msg} : ${p.t} (Lift level)")
          case _ =>
        }
        traversal.Continue(e, this)
      }
    })
    if (holeFound) {
      error("type holes were found")
    }
  }
}