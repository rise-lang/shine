package rise.core.types

import rise.core._
import rise.core.types.InferenceException.error
import rise.core.TypeLevelDSL._
import rise.core.primitives.Annotation

import scala.collection.mutable

// NOTE: We are in the process to removing this API
// Instead we are going to favour the TDSL API
object infer {
  def apply(e: Expr): Expr = {
    if (e.t != TypePlaceholder) {
      println("skipping type inference")
      return e
    }

    // build set of constraints
    val constraints = mutable.ArrayBuffer[Constraint]()
    val unique_e = uniqueNames.enforce(e)
    val typed_e = constrainTypes(unique_e, constraints, mutable.Map())
    // constraints.foreach(println)

    // solve the constraints
    // val bound = boundIdentifiers(typed_e)
    val solution = Constraint.solve(constraints, Seq())

    // apply the solution
    val r = solution(typed_e)
    printTypesAndTypeHoles(r)
    r
  }

  def constrainTypes(
      expr: Expr,
      constraints: mutable.ArrayBuffer[Constraint],
      env: mutable.Map[String, Type]
  ): Expr = {
    def typed(e: Expr): Expr = constrainTypes(e, constraints, env)
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
        tf

      case DepApp(f, x) =>
        val tf = typed(f)
        val exprT = genType(expr)
        val constraint = DepConstraint(tf.t, x, exprT)
        constraints += constraint
        DepApp(tf, x)(exprT)

      case Annotation(e, t) =>
        val te = typed(e)
        val constraint = TypeConstraint(te.t, t)
        constraints += constraint
        te

      case l: Literal => l

      case p: Primitive => p.setType(p.typeScheme)
    }
  }

  def printTypesAndTypeHoles(expr: Expr): Unit = {
    var holeFound = false
    traversal.DepthFirstLocalResult(
      expr,
      new traversal.Visitor {
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
      }
    )
    if (holeFound) {
      error("type holes were found")(Seq())
    }
  }
}
