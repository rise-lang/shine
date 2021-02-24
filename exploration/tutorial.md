# Exploration Tutorial

### Path to tutorial
*src/test/scala/exploration/exploration_tutorial.scala*

### Initialize RISE expression
// add RISE expression here

### Configuration
A very simple example. It combines the Iterative Improvement algorithm with the C Executor.
```json
  {
      "name" : "mm_example_iterative_improvement",
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
Another example. The Random algorithm on top combined with the C Executor.
```json
  {
      "name" : "mm_example_random",
      "strategies" : "default",
      "output" : "exploration",
      "inputSize" : 512,
      "metaheuristic" : [{
        "heuristic" : "Random",
        "depth" : 5,
        "iterations" : 10
      }],
      "executor" : {
        "name" : "C",
        "iterations" : 3,
	  	"threshold" : 10
      }
}
```
### Execution
Run `main` in *src/test/scala/exploration/exploration_tutorial.scala*