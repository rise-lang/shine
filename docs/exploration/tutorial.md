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

The Heuristic Search based Exploration allows to automatically optimize a
[High-Level RISE Program](/tutorial#high-level-program)
by applying rewrite rules (and more generally
[ELEVATE Optimization Strategies](/tutorial#optimization-strategy)) in an
explorative process that is guided by [Heuristic Search Algorithms](/exploration/exploration#heuristic-search-algorithms).

## Overview
In this basic tutorial we walk through an example on how to configure and
perform an exploration run.

We start with a definition of a High-Level RISE program for
matrix-matrix multiplication:
```scala mdoc:silent
// input size
val N = 512

// definition of matrix-matrix multiplication
// as a high-level program in RISE
val mm =
  fun(ArrayType(N, ArrayType(N, f32)))(a =>
    fun(ArrayType(N, ArrayType(N, f32)))(b =>
      a |> map(fun(ak =>
        b |> transpose |> map(fun(bk =>
          zip(ak)(bk) |>
            map(fun(x => fst(x) * snd(x))) |>
            reduce(add)(l(0.0f)) )) )) ))
```

Our goal is to perform an exploration by applying ELEVATE Optimization Strategies
to this program in an explorative process guided by Heuristic Search Algorithms.
For this we have to configure the exploration process.

## Configuration of Exploration Process
The exploration process is configured with a `JSON` configuration file.
We show two examples here.

#### Configuration using Iterative Improvement Search
First, a simple example using the iterative improvement
(aka, iterative refinement, or [local search](https://en.wikipedia.org/wiki/Metaheuristic#Local_search_vs._global_search)) heuristic search algorithm.
We also specify that we evaluate the performance function `f` that guides our
exploration process by compiling the rewritten program candidate to C and
executing it.

The corresponding `JSON` configuration file
```scala mdoc:passthrough
val iiFile = "exploration/configuration/mm_example_iterative_improvement.json"
println(s""" `"$iiFile"` """)
```
looks like this:
```scala mdoc:passthrough
printJsonFile(iiFile)
```

#### Configuration using Random Search
The second example is similar and uses the Random Search Heuristic.

The corresponding `JSON` configuration file
```scala mdoc:passthrough
val randomFile = "exploration/configuration/mm_example_random.json"
println(s""" `"$randomFile"` """)
```
looks like this:
```scala mdoc:passthrough
printJsonFile(randomFile)
```

## Start Exploration Process
Once we have provided a configuration we can simply start the exploration
process using the `riseExploration` function.

#### Perform Exploration with Iterative Improvement Search
```scala mdoc:compile-only
// run exploration with iterative improvement
val iterativeImprovementConfig =
    "exploration/configuration/mm_example_iterative_improvement.json"
riseExploration(mm, iterativeImprovementConfig)
```

#### Perform Exploration with Random Search
```scala mdoc:compile-only
// run exploration with random
val randomConfig =
    "exploration/configuration/mm_example_iterative_improvement.json"
riseExploration(mm, randomConfig)
```

## Output of Exploration Process
The exploration process will take a couple of minutes to complete.
Once complete, the folder specified as `output` in the configuration file
(in the examples above this is `"exploration"`) will contain a new subfolder
containing the results of the exploration process.
This subfolder contains:
 - a copy of the configuration file used for the exploration
 - a subfolder containing the results of every iteration of the Heuristic Search Algorithm
   - A `csv` file containing an overview of the runtime measured for each resulting program
   - A copy of every resulting program (in various forms)
   - A `dot` file to visualize the exploration procress 
   - a subfolder containing the results of the executor
      - A `csv` file containing the runtimes of every single evaluated program as part of the exploration process
      - A copy of every evaluated program (in various forms)

This information makes it easy to identify the best found expression, as well
as producing graphs investigating the exploration process itself. 
