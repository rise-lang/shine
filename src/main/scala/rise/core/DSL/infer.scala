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
        // FIXME: work around type annotation should not be a primitive bug
        case TypeAnnotation(e, _) => this.expr(e)
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

  type ExprEnv = Map[String, Type]

  def apply(e: Expr, exprEnv: ExprEnv = Map(), typeEnv : Set[Kind.Identifier] = Set(),
            printFlag: Flags.PrintTypesAndTypeHoles = Flags.PrintTypesAndTypeHoles.Off): Expr = {
    // TODO: Get rid of TypeAssertion and deprecate, instead evaluate !: in place and use `preserving` directly
    // Collect FTVs in assertions and opaques; transform assertions into annotations
    val (e_preserve, e_wo_assertions) = traverse(e, collectPreserve)
    // Collect constraints
    val (typed_e, constraints) = constrainTypes(exprEnv)(e_wo_assertions)
    // Collect free variables both before and after constraint gathering:
    // Some types in the collected constraints might not be present after constraint gathering (Fixme: BUG?)
    val ftvs = IsClosedForm.freeVars(e_wo_assertions)._2.toSet ++ IsClosedForm.freeVars(typed_e)._2.toSet
    val toSubstitute = (ftvs removedAll e_preserve) removedAll typeEnv
    // Solve constraints while preserving the FTVs in preserve
    val solution = Constraint.solve(constraints, toSubstitute, Seq())
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

    override def typeIdentifier[I <: Kind.Identifier]: VarType => I => Pair[I] = _ => {
      case i: Kind.Explicitness =>
        accumulate(if (i.isExplicit) Set(i) else Set())(i.asInstanceOf[I])
      case i => accumulate(Set())(i)
    }

    override def nat: Nat => Pair[Nat] = ae => {
      val pres = mutable.ListBuffer[Kind.Identifier]()
      val r = ae.visitAndRebuild({
        case i: NatIdentifier if i.isExplicit => pres += i; i
        case n => n
      })
      accumulate(pres.toSet)(r)
    }

    override def expr: Expr => Pair[Expr] = {
      // Transform assertions into annotations, collect FTVs
      case TypeAssertion(e, t) =>
        val (s1, e1) = expr(e).unwrap
        val (s2, _) = `type`(t).unwrap
        accumulate(s1 ++ s2 ++ getFTVs(t))(TypeAnnotation(e1, t) : Expr)
      // Collect FTVs
      case Opaque(e, t) =>
        val (s, _) = `type`(t).unwrap
        accumulate(s ++ getFTVs(t).toSet)(Opaque(e, t) : Expr)
      case e => super.expr(e)
    }
  }

  private val genType : Expr => Type =
    e => if (e.t == TypePlaceholder) freshTypeIdentifier else e.t
  private def ifTyped[T] : Type => T => Seq[T] =
    t => c => if (t == TypePlaceholder) Nil else Seq(c)

  def constrainTypes(exprEnv : ExprEnv) : Expr => (Expr, Seq[Constraint]) = {
    case i: Identifier =>
      val t = exprEnv.getOrElse(i.name,
        if (i.t == TypePlaceholder) error(s"$i has no type")(Seq()) else i.t )
      val c = TypeConstraint(t, i.t)
      (i.setType(t), Nil :+ c)

    case expr@Lambda(x, e) =>
      val tx = x.setType(genType(x))
      val exprEnv1 = exprEnv + (tx.name -> tx.t)
      val (te, cs) = constrainTypes(exprEnv1)(e)
      val ft = FunType(tx.t, te.t)
      val cs1 = ifTyped(expr.t)(TypeConstraint(expr.t, ft))
      (Lambda(tx, te)(ft), cs ++ cs1)

    case expr@App(f, e) =>
      val (tf, csF) = constrainTypes(exprEnv)(f)
      val (te, csE) = constrainTypes(exprEnv)(e)
      val exprT = genType(expr)
      val c = TypeConstraint(tf.t, FunType(te.t, exprT))
      (App(tf, te)(exprT), csF ++ csE :+ c)

    case expr@DepLambda(x, e) =>
      val (te, csE) = constrainTypes(exprEnv)(e)
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
      val csE1 = ifTyped(expr.t)(TypeConstraint(expr.t, tf.t))
      (tf, csE ++ csE1)

    case expr@DepApp(f, x) =>
      val (tf, csF) = constrainTypes(exprEnv)(f)
      val exprT = genType(expr)
      val c = DepConstraint(tf.t, x, exprT)
      (DepApp(tf, x)(exprT), csF :+ c)

    case TypeAnnotation(e, t) =>
      val (te, csE) = constrainTypes(exprEnv)(e)
      val c = TypeConstraint(te.t, t)
      (te, csE :+ c)

    case TypeAssertion(e, t) =>
      val (te, csE) = constrainTypes(exprEnv)(e)
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