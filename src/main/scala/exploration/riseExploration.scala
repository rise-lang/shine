package exploration

import elevate.heuristic_search.heuristics.Random
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
    val search = new Random[Rise](s, version)

    //start random search
    val result = search.start()

    //print result
    print("result: " + result)
  }
}
