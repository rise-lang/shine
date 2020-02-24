package exploration

import elevate.core.Strategy
import elevate.heuristic_search.heuristics.Random
import elevate.heuristic_search.heuristics.IterativeImprovement
import elevate.rise.Rise
import exploration.search.MockupSearch

object riseExploration {

  def apply(solution: Rise, lowering: Strategy[Rise]):Unit = {
    val s = solution
    print("initial solution: " + s + "\n")

    //search version
    val version = new MockupSearch(lowering)

    //heuristic
    val random = new Random[Rise](s, version)
    val iterativeImprovement = new IterativeImprovement[Rise](s, version)

    //start random search
    val resultIterativeImprovement = iterativeImprovement.start()
    val resultRandom = random.start()

    //print result
    println("result Random: " + resultRandom)
    println("result Iterative Improvement: " + resultIterativeImprovement)
  }
}
