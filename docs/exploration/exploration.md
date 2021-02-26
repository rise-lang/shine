---
title: Technical Overview of Heuristic Search based Exploration
sidebar_label: Exploration Overview
---

Our _Heuristic Search based Exploration_ allows to automatically optimize
[High-Level RISE Programs](tutorial#high-level-program) by applying rewrite rules
(and more generally _[ELEVATE Optimization Strategies](tutorial#optimization-strategy)_)
in an explorative process that is guided by _[Heuristic Search Algorithms](#heuristic-search-algorithms)_.

## Heuristic Search Algorithms
Heuristic search algorithms for optimisation problems are build up on three main components. 
- A solution `s`, which represents a possible solution for the optimisation problem
- A function `f`, which returns a numerical function value for a given solution `s`
- A function `N`, which returns a set of solutions called neighbourhood `N(s)` for a given solution `s`

The main principle of heurisitic search algorithms is the evaluation of the neighbourhood of the
current best solution by comparing the function values of neighbors, then picking the new best
solution and repeat the process. 
The individual heuristic search algorithms differ in their decision 
strategy on which solutions from the neighbourhood to choose. 

```scala mdoc:invisible
// General information about existing algorithms and their implementation:
// - [Iterative Improvement](#Iterative Improvement)
// - [Random](#Random)
// - more algorithms to come 
// 
// #### Iterative Improvement
// - description to come 
// 
// #### Random 
// - description to come
```
## Heuristics
The `Heuristic[P]` trait provides an abstraction across different Heuristic Search Algorithms,
such as _Iterative Improvement_ or _Random_.
The trait provides a single function `start` that each Heuristic Search Algorithm has to implement. 
```scala
def start(panel: HeuristicPanel[P],
          initialSolution: Solution[P],
          depth: Int): (P,              // The best found program
                        Option[Double], // The function value for the best program
                        Path[P]         // The path of rewritten expressions leading to the best program
                       )
```

## Heuristic panels
The `HeuristicPanel[P]` trait for provides the two main functions `f` and `N` mentioned above.
```scala
def f(solution: Solution[P]): Option[Double]
def N(solution: Solution[P]): Set[Solution[P]]
```

## Runner
The `Runner[P]` trait is used by the `HeuristicPanel[P]` implementation to
determine the function value `f(s)` for a given solution.
It provides a single `execute` function.
```scala
def execute(solution: Solution[P]): (P, Option[Double]) // Program and function value
```
A Runner is abstract. It has two realisations `Metaheuristic` and `Executor`.
The `Metaheuristic` runner starts a nested search heuristic to determine the function value.
The `Executor` runner directly executes the given solution.
This allows to nest and combine different heuristic search algorithms.
    
### Metaheuristic
A `Metaheuristic` is a realisation of a `Runner`.
It specifies the execution function of a heursitic search algorithm.
It can be specified with another metaheursitic as its `Runner`.
This way, different heuristics can be combined together to configure a nested exploration. 
A `Metaheuristic` is implemented in terms of: 
 - the `heuristic` algorithm used at this layer, 
 - the `depth` depth of the algorithm, e.g. how many steps a random algorithms goes down,
 - the number of `iterations`, e.g. it sometimes makes sense to repeat the execution (e.g. for for Random),
 - the `Runner` used as executor. This can be either a nested `Metaheuristic` or an `Executor`.

### Executor
The `Executor` represents the base-case of a `Metaheuristic` as it directly executes
and measures performance to determine the function value `f(s)` while the other
`Metaheuristics` start a nested heuristic search algorithm to determine `f(s)`. 
It is implemented specifically for `RISE` (while the `Metaheuristic` is generic)
and can be specified with: 
- the number of`iterations` to perform,
- a `threshold` factor (compared to the fastest executed runtime) to filter out expressions with slow execution times. 



