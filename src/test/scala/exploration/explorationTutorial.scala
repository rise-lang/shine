package exploration

import strategies.{blockingExploration, defaultStrategies}
import rise.core.DSL.{fun, lf32}
import rise.core.primitives._
import rise.core.types.DataType.{ArrayType, DataTypeIdentifier, f32}
import rise.elevate.rules.lowering.lowerToC
import rise.elevate.strategies.traversal.everywhere
import rise.elevate.rules.algorithmic.fuseReduceMap
import elevate.core._
import elevate.heuristic_search.util.Solution
import rise.autotune.HostCode
import rise.core.equality.{exprAlphaEq, typeAlphaEq, typeErasure}
import rise.core.{App, DepApp, DepLambda, Expr, Identifier, Lambda, Literal, Opaque, Primitive, TypeAnnotation, TypeAssertion}
import rise.elevate.Rise
import rise.elevate.rules.traversal.alternative
import rise.elevate.rules.traversal.default._
import rise.elevate.strategies.normalForm.DFNF
import rise.elevate.strategies.tiling.tile
import rise.elevate.strategies.traversal._

object explorationTutorial {
  // see: docs/exploration/tutorial.md
  // input size
  val N = 1 << 9

  // define matrix-matrix multiplication in RISE
  val mm =
    fun(ArrayType(N, ArrayType(N, f32)))(a =>
      fun(ArrayType(N, ArrayType(N, f32)))(b =>
        a |> map(fun(ak =>
          b |> transpose |> map(fun(bk =>
            zip(ak)(bk) |>
              map(fun(x => fst(x) * snd(x))) |>
              reduce(add)(lf32(0.0f))))))))

  // fuse reduce and map
  //  val mmsFused = (`map >> reduce -> reduce` `@` everywhere)(mm).get

  //  val lowering = fuseReduceMap `@` everywhere `;` lowerToC
  //  val lowering = lowerToC


  // dry run

  private val BENF = rise.elevate.strategies.normalForm.BENF()(alternative.RiseTraversable)

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
  def everywhere(s: Strategy[Rise]): ExpandStrategy = { p =>
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


  def rewriteFunction(solution: Solution[Rise]): Set[Solution[Rise]] = {

    // todo check try catch
    // todo add checking here?
    val rewritten: Seq[Solution[Rise]] = blockingExploration.rules.toSeq.flatMap(rule => {
      everywhere(rule).apply(solution.expression).map(e => Solution(e, solution.strategies :+ rule))
    })

    println("rewrite: " + rewritten.size)

    rewritten.toSet
  }

  def main(args: Array[String]): Unit = {

    // todo think about of applying normal form
    // todo think about checking
    // todo think about how to get this into rewriting via heuristics
    // e.g. pass function how to create expand strategy


    //
    //    val mm_tiled = expandStrategy.apply(mm_normal)
    //
    //    println("mm_tiled variants: " + mm_tiled.size)
    //
    //    println("mm: \n")
    //    mm_tiled.foreach(elem => println(DFNF().apply(elem).get))
    //
    //    println("\n\n")
    //    val mm_conservative = blockingExploration.blocking_step1.apply(mm).get
    //    println("mm_convervative: \n" + mm_conservative)

    // run exploration with iterative improvement
    //    riseExploration(mm, defaultStrategies.lowering, defaultStrategies.strategies, "exploration/configuration/mm_example_iterative_improvement.json")

    // heuristics

    // todo update exhaustive to tree structure?

    // todo make blocking parameter generic

    // todo add default case, if no tuning parameter was injected? Just tune fake parameter?

    riseExploration(
      mm,
      blockingExploration.lowering,
      blockingExploration.strategies,
      "exploration/configuration/mm/mm_example_exhaustive.json",
      rewriteFunction = Some(rewriteFunction),
      afterRewrite = Some(DFNF())
    )

    //    riseExploration(
    //      mm,
    //      blockingExploration.lowering,
    //      blockingExploration.strategies,
    //      "exploration/configuration/mm/mm_example_cot.json",
    //      rewriteFunction = Some(rewriteFunction),
    //      afterRewrite = Some(DFNF())
    //    )

    //
    //    riseExploration(
    //      mm,
    //      blockingExploration.lowering,
    //      blockingExploration.strategies,
    //      "exploration/configuration/mm/mm_example_autotuner.json",
    //      rewriteFunction = Some(rewriteFunction),
    //      afterRewrite = Some(DFNF())
    //    )

    //    riseExploration(
    //      mm,
    //      blockingExploration.lowering,
    //      blockingExploration.strategies,
    //      "exploration/configuration/mm/mm_example_autotuner.json",
    //      rewriteFunction = Some(rewriteFunction),
    //      afterRewrite = Some(DFNF())
    //    )


    //    riseExploration(mm, blockingExploration.lowering, blockingExploration.strategies, "exploration/configuration/mm/mm_example_iterative_improvement.json")
    //        riseExploration(mm, blockingExploration.lowering, blockingExploration.strategies, "exploration/configuration/mm/mm_example_exhaustive.json")
    //            riseExploration(mm, blockingExploration.lowering, blockingExploration.strategies, "exploration/configuration/mm/mm_example_random_sampling.json")
    //    riseExploration(mm, blockingExploration.lowering, blockingExploration.strategies, "exploration/configuration/mm/mm_example_random.json")

    // hm index

    // cot

    //    riseExploration(mm, lowering, defaultStrategies.strategies, "exploration/configuration/mm_example_random.json")

    // find results in exploration/ folder
  }

}

