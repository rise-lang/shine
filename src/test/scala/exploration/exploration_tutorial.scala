package exploration

import rise.core.DSL.{fun, l}
import rise.core.primitives._
import rise.core.types.{ArrayType, f32}

object explore {

  // input size
  val N = 1 << 9

  // define matrix-matrix multiplication in RISE
  val mm =
    fun(ArrayType(N, ArrayType(N, f32)))(a =>
      fun(ArrayType(N, ArrayType(N, f32)))(b =>
        map(fun(ak =>
          map(fun(bk =>
            (reduce(add)(l(0.0f)) o
              map(fun(x => fst(x) * snd(x)))) $
              zip(ak)(bk))) $ transpose(b) )) $ a))

  // fuse reduce and map
  val mm_fused = exploration.strategies.defaultStrategies.baseline(mm).get

  def main(args: Array[String]): Unit = {

    // run exploration with iterative improvement
    riseExploration(mm_fused, "exploration/configuration/mm_example_iterative_improvement.json")

    // run exploration with random
    riseExploration(mm_fused, "exploration/configuration/mm_example_random.json")

    // find results in exploration/
  }

}

