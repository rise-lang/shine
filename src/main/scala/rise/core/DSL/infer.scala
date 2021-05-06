package rise.core.DSL

import Type.freshTypeIdentifier
import rise.core._
import rise.core.traversal.{Continue, Result, Stop}
import rise.core.types.InferenceException.error
import rise.core.types._
import parser.{Span, parse}
import parser.parse.MapFkt

import scala.collection.mutable

object infer {
  private [DSL] def apply(e: Expr,mapFkt: Option[MapFkt]=None,
            printFlag: Flags.PrintTypesAndTypeHoles = Flags.PrintTypesAndTypeHoles.Off,
            explDep: Flags.ExplicitDependence = Flags.ExplicitDependence.Off): Expr = {
    val constraints = mutable.ArrayBuffer[Constraint]()
    val (typed_e, ftvSubs) = constrainTypes(e, constraints, mutable.Map(), mapFkt)
    println("Typed_E:\n"+ typed_e)
    println("constraints:\n"+ constraints)
    //println("ftvSubs:\n"+ ftvSubs)
    val solution = unfreeze(ftvSubs, Constraint.solve(constraints.toSeq, Seq())(explDep))
    val res = traversal.DepthFirstLocalResult(typed_e, Visitor(solution))
    if (printFlag == Flags.PrintTypesAndTypeHoles.On) {
      printTypesAndTypeHoles(res)
    }
    res
  }

  def printTypesAndTypeHoles(expr: Expr): Unit = {
    var holeFound = false
    traversal.DepthFirstLocalResult(
      expr,
      new traversal.Visitor {
        override def visitExpr(e: Expr): traversal.Result[Expr] = {
          e match {
            case h@primitives.typeHole(msg, None) =>
              println(s"found type hole ${msg}: ${h.t}")
              holeFound = true
            case p@primitives.printType(msg, None) =>
              println(s"$msg : ${p.t} (Rise level)")
            case _ =>
          }
          traversal.Continue(e, this)
        }
      }
    )
    if (holeFound) {
      error("type holes were found",expr.span)(Seq())
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
    traversal.types.DepthFirstLocalResult(
      t,
      new traversal.Visitor {
        override def visitType[T <: Type](t: T): Result[T] = {
          t match {
            case i: DataTypeIdentifier if !i.isExplicit => ftvs += i
            case i: TypeIdentifier                      => ftvs += i
            case _                                      =>
          }
          Continue(t, this)
        }
        override def visitNat(ae: Nat): Result[Nat] =
          Stop(ae.visitAndRebuild({
            case i: NatIdentifier if !i.isExplicit =>
              ftvs += i
              i
            case n => n
          }))
        override def visitAddressSpace(
                                        a: AddressSpace
                                      ): Result[AddressSpace] = {
          a match {
            case i: AddressSpaceIdentifier if !i.isExplicit => ftvs += i
            case _                                          =>
          }
          Continue(a, this)
        }
        override def visitN2N(n2n: NatToNat): Result[NatToNat] = {
          n2n match {
            case i: NatToNatIdentifier if !i.isExplicit => ftvs += i
            case _                                      =>
          }
          Continue(n2n, this)
        }
        override def visitN2D(n2d: NatToData): Result[NatToData] = {
          n2d match {
            case i: NatToDataIdentifier if !i.isExplicit => ftvs += i
            case _                                       =>
          }
          Continue(n2d, this)
        }
      }
    )
    ftvs.distinct.toSeq
  }

  private def constrainTypes(
                              expr: Expr,
                              constraints: mutable.ArrayBuffer[Constraint],
                              env: mutable.Map[String, Type],
                              mapFkt: Option[MapFkt]
                            ): (Expr, Solution) = {
    def constrained(e: Expr): (Expr, Solution) =
      constrainTypes(e, constraints, env, mapFkt)
    def genType(e: Expr): Type =
      if (e.t == TypePlaceholder) freshTypeIdentifier else e.t //if TypePlaceholder it returns names with numbers after else it returns the normal name
    val span = expr.span
    expr match {
      case i: Identifier =>
        val t = env.getOrElseUpdate(i.name,
          if (i.t == TypePlaceholder) {
            mapFkt match {
              case Some(mF) => mF.get(i.name) match {
                case Some(fkt) => fkt match { //Todo: We need the implementation at some point too, now we only use the Type
                  case parse.HMExpr(e) => {
                    val subExpr = e.toExprWithMapFkt(mF)
                    constraints += TypeConstraint(subExpr.t, subExpr.t, span)
                    return (subExpr, Solution())
                  }
                  case parse.HMType(t) => error(s"$i has a type but no implementation: "+ t, span)(Seq())
                  case parse.HMNat(parser.parse.SNat(nat, sp)) =>
                    //It creates an InferExp, because Nat needs to be combined with DepApp and not with App
                    //and because we are not able to change it here, this solution to put Nat here, can not work
                  {
                    constraints += TypeConstraint(NatType, NatType, span)
                    nat match {
                      case parse.NNumber(n) => return (n.toExpr, Solution())
                      case parse.NIdentifier(n) => error(s"only explicit values are accepted in declaring Nat: "+ n, span)(Seq())
                    }
                  }
                }
                case None =>error(s"$i has no type", span)(Seq())
              }
              case None =>error(s"$i has no type", span)(Seq())
            }
          } else {
            i.t
          })
        constraints += TypeConstraint(t, i.t, span)
        (i.setType(t), Solution())

      case lam@Lambda(x, e) =>
        val lamType = lam.t
        val tx = x.setType(genType(x))
        env.update(tx.name, tx.t)
        val (te, ftvSubsE) = constrained(e)
        env remove tx.name
        val ft = FunType(tx.t, te.t)
        val exprT = genType(expr)
        //val span = Span.combineOptionSpan(x.span,e.span)
        val constraint = TypeConstraint(exprT, ft, span)
        println("LambdaConstraint:\n"+ constraint)
        constraints += constraint
        println("LambdaConstraint appended:\n"+ constraints +"\n tx= '"+ tx + "'\n tx.type= ' "+ tx.t+
          "'\n TypeOfLambda= ' "+ lamType+
          "'\nte= '"+te+"' \nft= '"+ ft+ "'\n span = "+ span)

        (Lambda(tx, te)(ft, span), ftvSubsE)

      case App(f, e) =>
        val (tf, ftvSubsF) = constrained(f)
        val (te, ftvSubsE) = constrained(e)
        val exprT = genType(expr)
        val span = Span.combineOptionSpan(f.span,e.span)
        val constraint = TypeConstraint(tf.t, FunType(te.t, exprT),span)
        constraints += constraint
        (App(tf, te)(exprT, span), ftvSubsF <> ftvSubsE)

      case DepLambda(x, e) =>
        val (te, ftvSubsE) = constrained(e)
        val exprT = genType(expr)
        val tf = x match {
          case n: NatIdentifier =>
            DepLambda[NatKind](n, te)(DepFunType[NatKind, Type](n, te.t), span)
          case dt: DataTypeIdentifier =>
            DepLambda[DataKind](dt, te)(DepFunType[DataKind, Type](dt, te.t), span)
          case ad: AddressSpaceIdentifier =>
            DepLambda[AddressSpaceKind](ad, te)(
              DepFunType[AddressSpaceKind, Type](ad, te.t), span
            )
          case n2n: NatToNatIdentifier =>
            DepLambda[NatToNatKind](n2n, te)(
              DepFunType[NatToNatKind, Type](n2n, te.t), span
            )
        }
        val constraint = TypeConstraint(exprT, tf.t, span)
        constraints += constraint
        (tf, ftvSubsE)

      case DepApp(f, x) =>
        val (tf, ftvSubsF) = constrained(f)
        val exprT = genType(expr)
        val constraint = DepConstraint(tf.t, x, exprT,span)
        constraints += constraint
        (DepApp(tf, x)(exprT, span), ftvSubsF)

      case TypeAnnotation(e, t) =>
        val (te, ftvSubsE) = constrained(e)
        val constraint = TypeConstraint(te.t, t,span)
        constraints += constraint
        (te, ftvSubsE)

      case TypeAssertion(e, t) =>
        val ftvSubsT = getFTVSubs(t)
        val (te, ftvSubsE) = constrained(e)
        val constraint = TypeConstraint(te.t, freeze(ftvSubsT, t),span)
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

  private case class Visitor(sol: Solution) extends traversal.Visitor {
    override def visitExpr(e: Expr): Result[Expr] = e match {
      case Opaque(x, _) => Stop(x)
      case TopLevel(x, inst) =>
        Stop(traversal.DepthFirstLocalResult(x, TopLevel.Visitor(inst, sol)))
      case _ => Continue(e, this)
    }
    override def visitNat(ae: Nat): Result[Nat] = Stop(sol(ae))
    override def visitType[T <: Type](t: T): Result[T] =
      Stop(sol(t).asInstanceOf[T])
    override def visitAddressSpace(a: AddressSpace): Result[AddressSpace] =
      Stop(sol(a))
    override def visitN2D(n2d: NatToData): Result[NatToData] =
      Stop(sol(n2d))
    override def visitN2N(n2n: NatToNat): Result[NatToNat] =
      Stop(sol(n2n))
  }
}

object inferDependent {
  def apply(e: ToBeTyped[Expr],
            printFlag: Flags.PrintTypesAndTypeHoles = Flags.PrintTypesAndTypeHoles.Off): Expr = infer(e match {
    case ToBeTyped(e) => e
  }, None, printFlag, Flags.ExplicitDependence.On)
}
