package exploration

import rise.core.DSL.{fun, lf32}
import rise.core.primitives._
import rise.core.types.DataType.{ArrayType, f32}

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
              reduce(add)(lf32(0.0f)) )) )) ))

  // fuse reduce and map
//  val mmsFused = (`map >> reduce -> reduce` `@` everywhere)(mm).get

  def main(args: Array[String]): Unit = {

    // run exploration with iterative improvement
    riseExploration(mm, "exploration/configuration/mm_example_iterative_improvement.json")

    // run exploration with random
    riseExploration(mm, "exploration/configuration/mm_example_random.json")

    // find results in exploration/ folder
  }

}

