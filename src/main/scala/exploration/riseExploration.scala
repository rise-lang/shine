package exploration

import elevate.heuristic_search.heuristics.Random
import elevate.heuristic_search.heuristics.IterativeImprovement
import elevate.rise.Rise
import exploration.search.MockupSearch

object riseExploration {

  def main(solution: Rise): Unit = {
    val s = solution
    print("initial solution: " + s + "\n")

    //search version
    //maybe config with lowering and co
    val version = new MockupSearch()

    //heuristic
    val random = new Random[Rise](s, version)
    val iterativeImprovement = new IterativeImprovement[Rise](s, version)

    //start random search
    val resultIterativeImprovement = iterativeImprovement.start()
    val resultRandom = random.start()

    //print result
    println("result Iterative Improvement: " + resultIterativeImprovement)
    println("result Random: " + resultRandom)
  }
}
