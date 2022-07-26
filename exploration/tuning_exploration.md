
# Tuning Exploration

## Preparation
```bash
# setup shine
git checkout tuning_exploration
./setup.sh
sbt compile

# install hypermapper (auto tuner)
pip install hypermapper
```

## Run Exploration
 - Execute main in `src/test/scala/exploration/mvExploration.scala`
 - Find Results in `exploration/mv_autotuner*`

## Adjust Exploration

### Configuration File
 `exploration/configuration/mv_tuner.json`

```json
  {
      "name" : "mv_autotuner", <- name of experiment 
      "output" : "exploration", <- output folder
      "inputSize" : 1024, <- ignore this for now (input size is defiened hard-coded)
      "metaheuristic" : [{
        "heuristic" : "autotuner", <- we want to execute a solution using auto tuning
        "depth" : 7,  <- define depth of search space (limits the search space -> we might want to change this to number of nodes in the future to gain better control over the search space generation)
        "iterations" : 1 <- ignore this for now, we could rerun the auto tuning here 
      }],
      "executor" : {
        "name" : "AutoTuning",
        "iterations" : 10, <- ignore this for now
	    "threshold" : 10 <- ignore this for now 
      }
}

```

### Number of Iterations 
`lib/elevate/src/main/scala/elevate/heuristic_search/heuristics/AutotunerSearch.scala`
```scala
142    val doe = 10 // initialization of bo (keep it to 50%)
143    val optimizationIterations = 10 // bo 
```

### Search Space (used Strategies)
`src/main/scala/exploration/strategies/defaultStrategiesGPU.scala`
```scala
192   
193 	val strategies = Set[Strategy[Rise]](  // adjust this set #
194  		lowerGs0,  
195  		lowerGs1,  
196  		lowerWrg0,
```

### Debug Executor
To develop and test the tuning integration you can use the tuning executor. By default it minimizes the size of an expression. 

Swap commented lines (113 - 114) in `src/test/scala/exploration/mvExploration.scala`
```scala
//    riseExploration(mvHighLevel, defaultStrategiesGPU.lowering, defaultStrategiesGPU.strategies, "exploration/configuration/mv_tuner.json", Some(HostCode(mvHostCode.init(1024, 1024), mvHostCode.compute, mvHostCode.finish)))  
  riseExploration(mvHighLevel, defaultStrategiesGPU.lowering, defaultStrategiesGPU.strategies, "exploration/configuration/mv_tuner_debug.json", None)
```

Swap commented lines (156 - 158) in `src/main/scala/exploration/riseExploration.scala`
```scala
    // todo fix hard coded changes here!  
  // root metaheuristic using executor as executor  
  val rootChoice = result.metaheuristic.reverse.head  
    val rootMetaheuristic = new Metaheuristic[Rise](rootChoice.heuristic, jsonParser.getHeuristic(rootChoice.heuristic),  
//      rootChoice.depth,rootChoice.iteration, executor.asInstanceOf[AutoTuningExecutor], strategies, nameList.reverse.apply(index))  
//    val rootMetaheuristic = new Metaheuristic[Rise](rootChoice.heuristic, jsonParser.getHeuristic(rootChoice.heuristic),  
  rootChoice.depth,rootChoice.iteration, executor.asInstanceOf[DebugExecutor], strategies, nameList.reverse.apply(index))  
    index = index + 1
```

### Adjust
To use your own cost function you can have a look at `src/main/scala/exploration/runner/DebugExecutor.scala`


## First steps and Todos
- Get experiment pipeline to work
- Play around with configuration/Debug Executor
- Adjust host-code to read in data and check result (line 70) `src/test/scala/exploration/mvExploration.scala`

