# Exploration Tutorial

### Path to tutorial
*src/test/scala/exploration/exploration_tutorial.scala*

### Initialize RISE expression
```
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

```

### Configuration of Exploration
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

```
  def main(args: Array[String]): Unit = {

    // run exploration with iterative improvement
    riseExploration(mm_fused, "exploration/configuration/mm_example_iterative_improvement.json")

    // run exploration with random
    riseExploration(mm_fused, "exploration/configuration/mm_example_random.json")

    // find results in exploration/
  }
```