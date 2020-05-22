# Heuristic Search
Documentation about heuristic search 

## Components

## heuristic search algorithms
Heuristic search algorithms for optimisation problems are build up on three main components. 
- A solution `s`, which solves the optimisation problem
- A function `f(s)`, which maps a numerical function value to a solution `s`
- A function `N(s)`, which maps a set of solutions to a solution `s`

The main principle of heurisitic search algorithms is the evaluation of a solution's neighbourhood by comparing the 
function values, then picking a new current solution and repeat this process. The algorithms differ in their decision 
strategy, which solutions from a neighbourhood to choose. 

General information about algorithms, way to implement them. 
- Iterative Improvement (add link)
- Random (add link)
- more to come 

#### Iterative Improvement
describe 

#### Random
describe 

## heuristic panel
Panel for implementing heuristic search algorithms provides two main functions:

`Seq[solution]:N(s:solution)q and Option[Double]:f(s:solution)`

#### implementation/realization
This is a panel including things. Definition of Neighbourhood.

### Runner
An instance of the heursitic panel implementation is specified with a Runner. This class provides the following function:
```
  def execute(solution: P):(P,Option[Double])
```
This function is used for determination of the performance value of a heuristic. So for the specification of `f(solution)`.
A Runner is abstract (implemented as scala trait). It has two realizations `Metaheuristic` and `Executor`. While the 
`Metaheuristic` starts another search heuristic for function value, the `Executor` executes the given solution. This 
allows to nest and combine different heuristic search algorithms.
    
#### Metaheuristic
A Metaheuristic is the realization of a Runner. It specifies the execution function of a heursitic search algorithm. 
Concept of Metaheuristic. Stick together and nest heursitic algorithms to a metaheursitic. This represents an exploration. Use another heuristic as implementation of performance value for a heuristic. Therefore a metaheursitic element includes: 
 - `heuristic` the algorithm used at this layer 
 - `depth` the depth of the algorithm, e.g. how many steps a random algorithms goes down
 - `iterations` number of iterations, e.g. Random it can be senseful to repeat execution
 The executor can be another metaheuristic or an executor, which actually executes the expressions. 

#### Executor
Can be seen as bottom or root metaheuristic as it executes and measures performance to determine the function value `f`. 
It is implemented on the `shine` side. It can be specified with number of `iterations` and the `threshold` factor 
which filters out too slow expressions.

## Usage

### Example Exploration
A very simple example. It combines the Iterative Improvement algorithm with the C Executor.
```json
  {
      "name" : "dot_example",
      "strategies" : "default",
      "output" : "/home/jo/development/rise-lang/shine/exploration",
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

A more advanced example. It combines the Iterative Improvement algorithm on top. To determine the function value, a Random
algorithm is started with 5 iterations and a depth of 10. To determine the function value for Random, the C Executor is used with 10 iterations and a threshold of 10. 
 ```json
{
      "name" : "dot_example",
      "strategies" : "default",
      "output" : "/home/jo/development/rise-lang/shine/exploration",
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
