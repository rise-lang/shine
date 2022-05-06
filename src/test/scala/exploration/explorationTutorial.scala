package exploration

import strategies.{blockingExploration, defaultStrategies}
import rise.core.DSL.{fun, lf32}
import rise.core.primitives._
import rise.core.types.DataType.{ArrayType, f32}
import rise.elevate.rules.lowering.lowerToC
import rise.elevate.strategies.traversal.everywhere
import rise.elevate.rules.algorithmic.fuseReduceMap
import elevate.core._
import rise.elevate.rules.traversal.default._
import rise.elevate.strategies.traversal._

object explorationTutorial {
  // see: docs/exploration/tutorial.md
  // input size
  val N = 1 << 10

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

  def main(args: Array[String]): Unit = {

    // run exploration with iterative improvement
    //    riseExploration(mm, defaultStrategies.lowering, defaultStrategies.strategies, "exploration/configuration/mm_example_iterative_improvement.json")

    // heuristics

    // todo update exhaustive to tree strucutre?

    riseExploration(mm, blockingExploration.lowering, blockingExploration.strategies, "exploration/configuration/mm/mm_example_iterative_improvement.json")
    //    riseExploration(mm, blockingExploration.lowering, blockingExploration.strategies, "exploration/configuration/mm/mm_example_exhaustive.json")
    //    riseExploration(mm, blockingExploration.lowering, blockingExploration.strategies, "exploration/configuration/mm/mm_example_cot2.json")
    //    riseExploration(mm, blockingExploration.lowering, blockingExploration.strategies, "exploration/configuration/mm/mm_example_random_sampling.json")
    //    riseExploration(mm, blockingExploration.lowering, blockingExploration.strategies, "exploration/configuration/mm/mm_example_autotuner.json")
    //    riseExploration(mm, blockingExploration.lowering, blockingExploration.strategies, "exploration/configuration/mm/mm_example_random.json")

    // hm index

    // cot

    //    riseExploration(mm, lowering, defaultStrategies.strategies, "exploration/configuration/mm_example_random.json")

    // find results in exploration/ folder
  }

}

