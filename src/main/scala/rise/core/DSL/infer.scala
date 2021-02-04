package rise.core.DSL

import Type.freshTypeIdentifier
import rise.core.Traverse.{Pure, PureExprTraversal, PureTraversal}
import rise.core._
import rise.core.types.InferenceException.error
import rise.core.types._

import scala.collection.mutable

object infer {
  private [DSL] def apply(e: Expr,
            printFlag: Flags.PrintTypesAndTypeHoles = Flags.PrintTypesAndTypeHoles.Off,
            explDep: Flags.ExplicitDependence = Flags.ExplicitDependence.Off): Expr = {
    val constraints = mutable.ArrayBuffer[Constraint]()
    val (typed_e, ftvSubs) = constrainTypes(e, constraints, mutable.Map())
    val solution = unfreeze(ftvSubs, Constraint.solve(constraints.toSeq, Seq())(explDep))
    val res = Traverse(typed_e, Visitor(solution))
    if (printFlag == Flags.PrintTypesAndTypeHoles.On) {
      printTypesAndTypeHoles(res)
    }
    res
  }

  def printTypesAndTypeHoles(expr: Expr): Unit = {
    // TODO: move holeFound state into the traverse
    var holeFound = false
    Traverse(expr, new PureExprTraversal {
      override def expr : Expr => Pure[Expr] = {
        case h@primitives.typeHole(msg) =>
          println(s"found type hole ${msg}: ${h.t}")
          holeFound = true
          return_(h : Expr)
        case p@primitives.printType(msg) =>
          println(s"$msg : ${p.t} (Rise level)")
          return_(p : Expr)
        case e => super.expr(e)
      }
    })
    if (holeFound) {
      error("type holes were found")(Seq())
    }
  }

  implicit class TrivialSolutionConcat(a: Solution) {
    def <>(b: Solution): Solution =
      new Solution(
        a.ts ++ b.ts,
        a.ns ++ b.ns,
        a.as ++ b.as,
        a.n2ds ++ b.n2ds,
        a.n2ns ++ b.n2ns,
        a.natColls ++ b.natColls
      )
  }

  private def freeze(ftvSubs: Solution, t: Type): Type =
    Solution(
      ftvSubs.ts.view.mapValues(dt =>
        dt.asInstanceOf[DataTypeIdentifier].asExplicit).toMap,
      ftvSubs.ns.view.mapValues(n =>
        n.asInstanceOf[NatIdentifier].asExplicit).toMap,
      ftvSubs.as.view.mapValues(a =>
        a.asInstanceOf[AddressSpaceIdentifier].asExplicit).toMap,
      ftvSubs.n2ds.view.mapValues(n2d =>
        n2d.asInstanceOf[NatToDataIdentifier].asExplicit).toMap,
      ftvSubs.n2ns.view.mapValues(n2n =>
        n2n.asInstanceOf[NatToNatIdentifier].asExplicit).toMap,
      ftvSubs.natColls.view.mapValues(natColl =>
        natColl.asInstanceOf[NatCollectionIdentifier].asExplicit).toMap
    )(t)

  private def unfreeze(ftvSubs: Solution, solution: Solution): Solution = solution match {
    case Solution(ts, ns, as, n2ds, n2ns, natColls) =>
      Solution(
        ts.view.mapValues(t => ftvSubs(t)).toMap,
        ns.view.mapValues(n => ftvSubs(n)).toMap,
        as.view.mapValues(a => ftvSubs(a)).toMap,
        n2ds.view.mapValues(n2d => ftvSubs(n2d)).toMap,
        n2ns.view.mapValues(n2n => ftvSubs(n2n)).toMap,
        natColls.view.mapValues(ftvSubs(_)).toMap
      )
  }

  private def getFTVSubs(t: Type): Solution = {
    import scala.collection.immutable.Map
    getFTVs(t).foldLeft(Solution())((solution, ftv) =>
      solution match {
        case s@Solution(ts, ns, as, n2ds, n2ns, natColls) =>
          ftv match {
            case _: TypeIdentifier =>
              throw TypeException("TypeIdentifier cannot be frozen")
            case i: DataTypeIdentifier      => s.copy(ts = ts ++ Map(i -> i))
            case i: NatIdentifier           => s.copy(ns = ns ++ Map(i -> i))
            case i: AddressSpaceIdentifier  => s.copy(as = as ++ Map(i -> i))
            case i: NatToDataIdentifier     => s.copy(n2ds = n2ds ++ Map(i -> i))
            case i: NatToNatIdentifier      => s.copy(n2ns = n2ns ++ Map(i -> i))
            case i: NatCollectionIdentifier => s.copy(natColls = natColls ++ Map(i -> i))
            case i =>
              throw TypeException(s"${i.getClass} is not supported yet")
          }
      }
    )
  }

  def getFTVs(t: Type): Seq[Kind.Identifier] = {
    val ftvs = mutable.ListBuffer[Kind.Identifier]()
    Traverse(t, new PureTraversal {
        override def typeIdentifier[I <: Kind.Identifier]: I => Pure[I] = i => {
          i match {
            case i: Kind.Explicitness => if (!i.isExplicit) (ftvs += i)
            case i => ftvs += i
          }
          return_(i)
        }
    })
    ftvs.distinct.toSeq
  }

  private def constrainTypes(
                              expr: Expr,
                              constraints: mutable.ArrayBuffer[Constraint],
                              env: mutable.Map[String, Type]
                            ): (Expr, Solution) = {
    def constrained(e: Expr): (Expr, Solution) =
      constrainTypes(e, constraints, env)
    def genType(e: Expr): Type =
      if (e.t == TypePlaceholder) freshTypeIdentifier else e.t

    expr match {
      case i: Identifier =>
        val t = env.getOrElseUpdate(i.name,
          if (i.t == TypePlaceholder) {
            error(s"$i has no type")(Seq())
          } else {
            i.t
          })
        constraints += TypeConstraint(t, i.t)
        (i.setType(t), Solution())

      case Lambda(x, e) =>
        val tx = x.setType(genType(x))
        env.update(tx.name, tx.t)
        val (te, ftvSubsE) = constrained(e)
        env remove tx.name
        val ft = FunType(tx.t, te.t)
        val exprT = genType(expr)
        val constraint = TypeConstraint(exprT, ft)
        constraints += constraint
        (Lambda(tx, te)(ft), ftvSubsE)

      case App(f, e) =>
        val (tf, ftvSubsF) = constrained(f)
        val (te, ftvSubsE) = constrained(e)
        val exprT = genType(expr)
        val constraint = TypeConstraint(tf.t, FunType(te.t, exprT))
        constraints += constraint
        (App(tf, te)(exprT), ftvSubsF <> ftvSubsE)

      case DepLambda(x, e) =>
        val (te, ftvSubsE) = constrained(e)
        val exprT = genType(expr)
        val tf = x match {
          case n: NatIdentifier =>
            DepLambda[NatKind](n, te)(DepFunType[NatKind, Type](n, te.t))
          case dt: DataTypeIdentifier =>
            DepLambda[DataKind](dt, te)(DepFunType[DataKind, Type](dt, te.t))
          case ad: AddressSpaceIdentifier =>
            DepLambda[AddressSpaceKind](ad, te)(
              DepFunType[AddressSpaceKind, Type](ad, te.t)
            )
          case n2n: NatToNatIdentifier =>
            DepLambda[NatToNatKind](n2n, te)(
              DepFunType[NatToNatKind, Type](n2n, te.t)
            )
        }
        val constraint = TypeConstraint(exprT, tf.t)
        constraints += constraint
        (tf, ftvSubsE)

      case DepApp(f, x) =>
        val (tf, ftvSubsF) = constrained(f)
        val exprT = genType(expr)
        val constraint = DepConstraint(tf.t, x, exprT)
        constraints += constraint
        (DepApp(tf, x)(exprT), ftvSubsF)

      case TypeAnnotation(e, t) =>
        val (te, ftvSubsE) = constrained(e)
        val constraint = TypeConstraint(te.t, t)
        constraints += constraint
        (te, ftvSubsE)

      case TypeAssertion(e, t) =>
        val ftvSubsT = getFTVSubs(t)
        val (te, ftvSubsE) = constrained(e)
        val constraint = TypeConstraint(te.t, freeze(ftvSubsT, t))
        constraints += constraint
        (te, ftvSubsE <> ftvSubsT)

      case o: Opaque =>
        val ftvSubs = getFTVSubs(o.t)
        val frozenExpr = Opaque(o.e, freeze(ftvSubs, o.t))
        (frozenExpr, ftvSubs)

      case l: Literal => (l, Solution())

      case p: Primitive => (p.setType(p.typeScheme), Solution())
    }
  }

  private case class Visitor(sol: Solution) extends PureTraversal {
    override def expr : Expr => Pure[Expr] = {
      case Opaque(x, _) => return_(x)
      case TopLevel(x, inst) => TopLevel.Visitor(inst, sol).expr(x)
      case e => super.expr(e)
    }
    override def nat : Nat => Pure[Nat] = n => return_(sol(n))
    override def etype[T <: Type] : T => Pure[T] = t => return_(sol(t).asInstanceOf[T])
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
