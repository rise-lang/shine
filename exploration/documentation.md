# Heuristic Search
Documentation about heuristic search 

## Components

## heuristic search algorithms
Heuristic search algorithms for optimisation problems are build up on three main components. 
- A solution `s`, which solves the optimisation problem
- A function `f(s)`, which maps a numerical function value to a solution `s`
- A function `N(s)`, which returns a set of solutions called neighbourhood `N(s)` of a solution `s`

The main principle of heurisitic search algorithms is the evaluation of a solution's neighbourhood by comparing the 
function values, then picking a new current solution and repeat this process. 
The algorithms differ in their decision 
strategy which solutions from a neighbourhood to choose. 

General information about existing algorithms and their implementation:
- [Iterative Improvement](#Iterative Improvement)
- [Random](#Random)
- more algorithms to come 

#### Iterative Improvement
- description to come 

#### Random 
- description to come 

## heuristic panel
Panel for implementing heuristic search algorithms provides two main functions implemented as *Scala trait*:

- `Seq[solution]:N(s:solution)` 

- `Option[Double]:f(s:solution)`

### Runner
An instance of the heursitic panel implementation is specified with a Runner. This class provides the following function:
```
  def execute(solution: P):(P,Option[Double])
```
This function specifies the function `f(solution)` of the heuristic panel.
A Runner is abstract. It has two realisations `Metaheuristic` and `Executor`. While the 
`Metaheuristic` starts another search heuristic for function value, the `Executor` executes the given solution. This 
allows to nest and combine different heuristic search algorithms.
    
#### Metaheuristic
A *Metaheuristic* is the realisation of a *Runner*. It specifies the execution function of a heursitic search algorithm.
It can be specified with another metaheursitic as *Runner*.
This way, different heuristics can be sticked together to configure an exploration. 
A *Metaheuristic* element includes: 
 - `heuristic` the algorithm used at this layer 
 - `depth` the depth of the algorithm, e.g. how many steps a random algorithms goes down
 - `iterations` number of iterations, e.g. it make sense for Random to repeat the execution
 - `Runner` the used executor. This can be either a *Metaheuristic* or an *Executor*

#### Executor
The *Executor* represents the root *Metaheuristic* as it executes and measures performance to determine the function value `f(s)` while the other *Metaheuristics* start another heuristic search algorithm to determine `f(s)`. 
It is implemented on the `shine` side and can be specified with: 
- `iterations` number of iterations to perform
- `threshold` factor (compared to fastest executed runtime) to filter out slow expressions 

## Usage

### Example Exploration
A very simple example. It combines the Iterative Improvement algorithm with the C Executor.
```json
  {
      "name" : "dot_example",
      "strategies" : "default",
      "output" : "exploration",
      "inputSize" : 512,
      "metaheuristic" : [{
        "heuristic" : "IterativeImprovement",
        "depth" : 0,
        "iterations" : 1
      }],
      "executor" : {
        "name" : "C",
        "iterations" : 3,
	"threshold" : 10
      }
}
```

A more advanced example. It uses the Iterative Improvement algorithm on the highest layer. To determine the function value, a Random
algorithm is started with 5 iterations and a depth of 10. To determine the function value for Random the C Executor is used with 10 iterations and a threshold of 10. 
 ```json
{
      "name" : "dot_example",
      "strategies" : "default",
      "output" : "exploration",
      "inputSize" : 512,
      "metaheuristic" : [{
        "heuristic" : "IterativeImprovement",
        "depth" : 0,
        "iterations" : 1
      }, {
        "heuristic" : "Random",
        "depth" : 10,
        "iterations" : 5
      }],
      "executor" : {
        "name" : "C",
        "iterations" : 9,
	    "threshold" : 10
      }
}
```
