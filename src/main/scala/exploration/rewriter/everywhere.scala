package exploration.rewriter

import rise.core.DSL.{fun, lf32}
import rise.core.primitives._
import rise.core.types.DataType.{ArrayType, DataTypeIdentifier, f32}
import rise.elevate.rules.lowering.lowerToC
import rise.elevate.rules.algorithmic.fuseReduceMap
import elevate.core._
import elevate.heuristic_search.util.{Solution, SolutionStep, hashProgram, hashSolution}
import rise.autotune.HostCode
import rise.core.equality.{exprAlphaEq, typeAlphaEq, typeErasure}
import rise.core.{App, DepApp, DepLambda, Expr, Identifier, Lambda, Literal, Opaque, Primitive, TypeAnnotation, TypeAssertion}
import rise.elevate.Rise
import rise.elevate.rules.traversal.alternative
import rise.elevate.rules.traversal.default._
import rise.elevate.strategies.normalForm.DFNF
import rise.elevate.strategies.tiling.tile
import rise.elevate.strategies.traversal._


import scala.collection.parallel.CollectionConverters._

import java.io.{File, FileOutputStream, PrintWriter}
import java.io.{File, FileInputStream, FileReader}
import rise.core.DSL.ToBeTyped
import rise.core.Expr

import scala.reflect.runtime._
import scala.tools.reflect.ToolBox

object everywhere {


  case class ExprWrapper(e: Expr) {
    override def hashCode(): Int = exprAlphaEq(typeErasure).hash(e)

    override def equals(o: Any): Boolean = o match {
      case other: ExprWrapper => exprAlphaEq(typeAlphaEq).apply(this.e)(other.e)
      case other: Expr => exprAlphaEq(typeAlphaEq).apply(this.e)(other)
      case _ => false
    }
  }

  private def mayApply(s: Strategy[Rise], p: Rise): Option[Rise] = {
    s(p) match {
      case Success(p) => Some(p)
      case Failure(_) => None
    }
  }

  type ExpandStrategy = Rise => Seq[Rise]

  // todo check if we can extract the traversal from this
  // todo check if all possible locations are covered by this function
  var counter = 0

  // todo improve performance of this
  def everywhere(s: Strategy[Rise]): ExpandStrategy = { p =>
    counter += 1
    //    println(s"everywhere: [${counter}]")
    import rise.core.types._
    mayApply(s, p).toSeq ++ (p match {
      case App(f, e) => everywhere(s)(f).map(App(_, e)(p.t)) ++ everywhere(s)(e).map(App(f, _)(p.t))
      case Identifier(_) => Nil
      case Lambda(x, e) => everywhere(s)(e).map(Lambda(x, _)(p.t))
      case DepLambda(_, x, e) => x match {
        case n: NatIdentifier =>
          everywhere(s)(e).map(DepLambda(NatKind, n, _)(p.t))
        case n: DataTypeIdentifier =>
          everywhere(s)(e).map(DepLambda(DataKind, n, _)(p.t))
        case n: AddressSpaceIdentifier =>
          everywhere(s)(e).map(DepLambda(AddressSpaceKind, n, _)(p.t))
      }
      case DepApp(kind, f, x) => everywhere(s)(f).map(DepApp(kind, _, x)(p.t))
      case Literal(_) => Nil
      case _: TypeAnnotation => throw new Exception("Type annotations should be gone.")
      case _: TypeAssertion => throw new Exception("Type assertions should be gone.")
      case _: Opaque => throw new Exception("Opaque expressions should be gone.")
      case _: Primitive => Nil
    })
  }

  // todo magic!

  // todo check if parallel works here properly
  def rewriteFunction(strategies: Seq[Strategy[Rise]])(solution: Solution[Rise]): Seq[Solution[Rise]] = {

    // todo check try catch
    // todo add checking here?
    //    val rewritten: Seq[Solution[Rise]] = strategies.toSeq.par.flatMap(rule => {


    val rewritten: Seq[Solution[Rise]] = strategies.flatMap(rule => {
      //      println("try: " + rule)
      val rewrites = everywhere(rule).apply(solution.expression())

      val solutions: Seq[Solution[Rise]] = Range(0, rewrites.size).zip(rewrites).map(elem => {

        // create new step
        val step = SolutionStep[Rise](
          expression = elem._2,
          strategy = rule,
          location = elem._1
        )

        Solution[Rise](
          solutionSteps = solution.solutionSteps :+ step
        )
      })
      solutions
    })

    // add id rewrite at the end
    val idSolution = Solution[Rise](
      solutionSteps = solution.solutionSteps :+ SolutionStep[Rise](
        expression = solution.expression(),
        strategy = elevate.core.strategies.basic.id[Rise],
        location = 0
      )
    )

    val output = rewritten :+ idSolution

    output
  }


  //  def rewriteFunction(solution: Solution[Rise]): scala.collection.immutable.Seq[Solution[Rise]] = {
  //
  //    // todo check try catch
  //    // todo add checking here?
  //    val rewritten: scala.collection.immutable.Seq[Solution[Rise]] = exploration.strategies.blockingExploration.rules.map(rule => {
  //      //      println("try: " + rule)
  //      everywhere(rule).apply(solution.expression).map(e => Solution(e, solution.strategies :+ rule))
  //    }).flatten
  //
  //    println("rewrite: " + rewritten.size)
  //
  //    rewritten
  //  }


}
