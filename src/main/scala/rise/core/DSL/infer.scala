package rise.core.DSL

import util.monads._
import Type.freshTypeIdentifier
import rise.core.traverse._
import rise.core.{traverse => _, _}
import rise.core.types.InferenceException.error
import rise.core.types._

import scala.collection.mutable

object infer {
  def collectFreeEnv(e: Expr): Map[String, Type] = {
    case class Traversal(bound: Set[String]) extends PureAccumulatorTraversal[Map[String, Type]] {
      override val accumulator = MapMonoid

      override def expr: Expr => Pair[Expr] = {
        case Lambda(x, e) => this.copy(bound = bound + x.name).expr(e)
        case e => super.expr(e)
      }

      override def identifier[I <: Identifier]: VarType => I => Pair[I] =
        _ => i => {
          if (!bound(i.name)) {
            accumulate(Map(i.name -> i.t))(i)
          } else {
            return_(i)
          }
        }
    }

    traverse(e, Traversal(Set()))._1
  }

  def preservingWithEnv(e: Expr, env: Map[String, Type], preserve: Set[Kind.Identifier]): Expr = {
    val (typed_e, constraints) = constrainTypes(env)(e)
    val solution = Constraint.solve(constraints, preserve, Seq())(
      Flags.ExplicitDependence.Off)
    solution(typed_e)
  }

  // TODO: Get rid of TypeAssertion and deprecate, instead evaluate !: in place and use `preserving` directly
  private [DSL] def apply(e: Expr,
                          printFlag: Flags.PrintTypesAndTypeHoles = Flags.PrintTypesAndTypeHoles.Off,
                          explDep: Flags.ExplicitDependence = Flags.ExplicitDependence.Off): Expr = {
    // Collect FTVs in assertions and opaques; transform assertions into annotations
    val (preserve, e_wo_assertions) = traverse(e, collectPreserve)
    infer.preserving(e_wo_assertions, preserve, printFlag, explDep)
  }

  private [DSL] def preserving(wo_assertions: Expr, preserve : Set[Kind.Identifier],
                               printFlag: Flags.PrintTypesAndTypeHoles = Flags.PrintTypesAndTypeHoles.Off,
                               explDep: Flags.ExplicitDependence = Flags.ExplicitDependence.Off): Expr = {
    // Collect constraints
    val (typed_e, constraints) = constrainTypes(Map())(wo_assertions)
    // Solve constraints while preserving the FTVs in preserve
    val solution = Constraint.solve(constraints, preserve, Seq())(explDep)
    // Apply the solution
    val res = traverse(typed_e, Visitor(solution))
    if (printFlag == Flags.PrintTypesAndTypeHoles.On) {
      printTypesAndTypeHoles(res)
    }
    res
  }

  def printTypesAndTypeHoles(expr: Expr): Unit = {
    val hasHoles = new PureAccumulatorTraversal[Boolean] {
      override val accumulator = OrMonoid
      override def expr: Expr => Pair[Expr] = {
        case h@primitives.typeHole(msg) =>
          println(s"found type hole ${msg}: ${h.t}")
          accumulate(true)(h : Expr)
        case p@primitives.printType(msg) =>
          println(s"$msg : ${p.t} (Rise level)")
          return_(p: Expr)
        case e => super.expr(e)
      }
    }
    if (traverse(expr, hasHoles)._1) {
      error("type holes were found")(Seq())
    }
  }

  val FTVGathering = new PureAccumulatorTraversal[Seq[Kind.Identifier]] {
    override val accumulator = SeqMonoid
    override def typeIdentifier[I <: Kind.Identifier]: VarType => I => Pair[I] = _ => {
      case i: Kind.Explicitness => accumulate(if (!i.isExplicit) Seq(i) else Seq())(i.asInstanceOf[I])
      case i => accumulate(Seq(i))(i)
    }
    override def nat: Nat => Pair[Nat] = ae => {
      val ftvs = mutable.ListBuffer[Kind.Identifier]()
      val r = ae.visitAndRebuild({
        case i: NatIdentifier if !i.isExplicit => ftvs += i; i
        case n => n
      })
      accumulate(ftvs.toSeq)(r)
    }
  }

  def getFTVs(t: Type): Seq[Kind.Identifier] = {
    traverse(t, FTVGathering)._1.distinct
  }

  def getFTVsRec(e: Expr): Seq[Kind.Identifier] = {
    traverse(e, FTVGathering)._1.distinct
  }

  private val collectPreserve = new PureAccumulatorTraversal[Set[Kind.Identifier]] {
    override val accumulator = SetMonoid
    override def expr: Expr => Pair[Expr] = {
      // Transform assertions into annotations, collect FTVs
      case TypeAssertion(e, t) =>
        val (s, e1) = expr(e).unwrap
        accumulate(s ++ getFTVs(t))(TypeAnnotation(e1, t) : Expr)
      // Collect FTVs
      case Opaque(e, t) =>
        accumulate(getFTVs(t).toSet)(Opaque(e, t) : Expr)
      case e => super.expr(e)
    }
  }

  private val genType : Expr => Type =
    e => if (e.t == TypePlaceholder) freshTypeIdentifier else e.t
  private val constrIfTyped : Type => Constraint => Seq[Constraint] =
    t => c => if (t == TypePlaceholder) Nil else Seq(c)

  private val constrainTypes : Map[String, Type] => Expr => (Expr, Seq[Constraint]) = env => {
    case i: Identifier =>
      val t = env.getOrElse(i.name,
        if (i.t == TypePlaceholder) error(s"$i has no type")(Seq()) else i.t )
      val c = TypeConstraint(t, i.t)
      (i.setType(t), Nil :+ c)

    case expr@Lambda(x, e) =>
      val tx = x.setType(genType(x))
      val env1 : Map[String, Type] = env + (tx.name -> tx.t)
      val (te, cs) = constrainTypes(env1)(e)
      val ft = FunType(tx.t, te.t)
      val cs1 = constrIfTyped(expr.t)(TypeConstraint(expr.t, ft))
      (Lambda(tx, te)(ft), cs ++ cs1)

    case expr@App(f, e) =>
      val (tf, csF) = constrainTypes(env)(f)
      val (te, csE) = constrainTypes(env)(e)
      val exprT = genType(expr)
      val c = TypeConstraint(tf.t, FunType(te.t, exprT))
      (App(tf, te)(exprT), csF ++ csE :+ c)

    case expr@DepLambda(x, e) =>
      val (te, csE) = constrainTypes(env)(e)
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
      val csE1 = constrIfTyped(expr.t)(TypeConstraint(expr.t, tf.t))
      (tf, csE ++ csE1)

    case expr@DepApp(f, x) =>
      val (tf, csF) = constrainTypes(env)(f)
      val exprT = genType(expr)
      val c = DepConstraint(tf.t, x, exprT)
      (DepApp(tf, x)(exprT), csF :+ c)

    case TypeAnnotation(e, t) =>
      val (te, csE) = constrainTypes(env)(e)
      val c = TypeConstraint(te.t, t)
      (te, csE :+ c)

    case TypeAssertion(e, t) =>
      val (te, csE) = constrainTypes(env)(e)
      val c = TypeConstraint(te.t, t)
      (te, csE :+ c)

    case o: Opaque => (o, Nil)
    case l: Literal => (l, Nil)
    case p: Primitive => (p.setType(p.typeScheme), Nil)
  }

  private case class Visitor(sol: Solution) extends PureTraversal {
    override def expr : Expr => Pure[Expr] = {
      case Opaque(x, _) => return_(x)
      case TopLevel(x, inst) => TopLevel.Visitor(inst, sol).expr(x)
      case e => super.expr(e)
    }
    override def nat : Nat => Pure[Nat] = n => return_(sol(n))
    override def `type`[T <: Type] : T => Pure[T] = t => return_(sol(t).asInstanceOf[T])
    override def addressSpace : AddressSpace => Pure[AddressSpace] = a => return_(sol(a))
    override def natToData : NatToData => Pure[NatToData] = n2d => return_(sol(n2d))
    override def natToNat : NatToNat => Pure[NatToNat] = n2n => return_(sol(n2n))
  }
}

object inferDependent {
  def apply(e: ToBeTyped[Expr],
            printFlag: Flags.PrintTypesAndTypeHoles = Flags.PrintTypesAndTypeHoles.Off): Expr = infer(e match {
    case ToBeTyped(e) => e
  }, printFlag, Flags.ExplicitDependence.On)
}
