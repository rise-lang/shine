package exploration

import elevate.core.Strategy
import elevate.heuristic_search.heuristics.Random
import elevate.rise.Rise
import exploration.search.{MockupSearch, executeC}


// runner class
class Runner (val name:String,
              val depth:Int,
              val iterations: Int,
              val runner:Runner,
              val strategies:Set[Strategy[Rise]]
             ) {

  def execute(solution: Rise): Option[Double] = {

    // traverse runner path
    name match {
      case "C" => {
        // lower
        val lowered = elevate.rise.rules.lowering.lowerToC.apply(solution)
        //execute
        executeC(lowered, iterations)
      }
      case "OpenMP" => throw new Exception("not yet implemented")
      case "OpenCL" => throw new Exception("not yet implemented")
      case "Random" => {
        println("start new search from: " + name)
        var best:Option[Double] = None

        // repeat random for iterations time
        for(_ <- Range(0,iterations)){

          // new MockupSearch instance
          val version = new MockupSearch(runner, strategies)

          // start random heuristic
          val heuristic = new Random[Rise](solution, version, iterations)
          heuristic.start()

          // check for new best value (and/or initialize best variable)
          (best, heuristic.best) match {
            case (Some(a), Some(b)) => {
              if (b < a) {
                best = heuristic.best
              }
            }
            case (None, Some(b)) =>{
              best = Some(b)
            }
            case _ =>
          }
        }
        best
      }
      case _ => {
        throw new Exception("should never reach this point")
      }
    }
  }
}

