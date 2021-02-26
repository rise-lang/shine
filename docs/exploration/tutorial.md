---
title: A Tutorial of Heuristic Search based Exploration in RISE
sidebar_label: Tutorial of Heurisitc Search based Exploration
---
```scala mdoc:invisible
import rise.core.DSL.{fun, l}
import rise.core.primitives._
import rise.core.types.{ArrayType, f32}
import rise.elevate.rules.algorithmic._
import rise.elevate.strategies.traversal._
import exploration.explorationUtil.jsonParser
import exploration.riseExploration

import scala.io.Source
import scala.util.Using

def printJsonFile(file: String): Unit = {
    val json =
      Using(Source.fromFile(file)) {
        _.getLines().mkString("\n")
      }.get
    println("```json")
    println(s"$json")
    println("```")
}
```

### Initialize RISE expression
```scala mdoc:silent
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
            reduce(add)(l(0.0f)) )) )) ))
```

```scala mdoc:silent
// fuse reduce and map
val mmsFused =
    (`map >> reduce -> reduce` `@` everywhere)(mm).get
```

### Configuration of Exploration
A very simple example. It combines the Iterative Improvement algorithm with the C Executor.
```scala mdoc:passthrough
val iiFile = "exploration/configuration/mm_example_iterative_improvement.json"
println(s""" `"$iiFile"` """)
printJsonFile(iiFile)
```
```scala mdoc:compile-only
// run exploration with iterative improvement
val iterativeImprovementConfig =
    "exploration/configuration/mm_example_iterative_improvement.json"
riseExploration(mmsFused, iterativeImprovementConfig)
```

Another example. The Random algorithm on top combined with the C Executor.
```scala mdoc:passthrough
val randomFile = "exploration/configuration/mm_example_random.json"
println(s""" `"$randomFile"` """)
printJsonFile(randomFile)
```

```scala mdoc:compile-only
// run exploration with random
val randomConfig =
    "exploration/configuration/mm_example_iterative_improvement.json"
riseExploration(mmsFused, randomConfig)
```
