package exploration.neighborhoods

import elevate.heuristic_search.HeuristicPanel
import elevate.heuristic_search.util.Solution
import elevate.heuristic_search.Runner
import elevate.core._
import elevate.heuristic_search.util._

import rise.elevate.Rise
import exploration.rewriter.everywhere._

/**
  * Neighborhood is defined by distance in the tree (how many paths/steps take)
  * around leafs (fill with id)
  * All solutions are on leaf layer
  */
case class NTreeLeafsDistance(
                               val runner: Runner[Rise],
                               val strategies: Seq[Strategy[Rise]],
                               val afterRewrite: Option[Strategy[Rise]] = None
                             ) extends HeuristicPanel[Rise] {

  val solutions = new scala.collection.mutable.HashMap[String, Option[Double]]()

  // todo design and implement this
  // new approach requires thinking
  override def N(solution: Solution[Rise]): Seq[Solution[Rise]] = {

    throw new Exception("not yet implemented properly")

    // create neighborhood based on slide-window approach
    // don't use size of neighborhood as limit, use distance (in tree)

    // could change neighborhood size during the search
    // could add elements from other subtrees randomly

    null
  }

  override def f(solution: Solution[Rise]): Option[Double] = {
    // buffer performance values in hashmap
    solutions.get(hashSolution(solution)) match {
      case Some(value) => value
      case _ => {
        val performanceValue = runner.execute(solution).performance
        solutions.+=(hashSolution(solution) -> performanceValue)
        performanceValue
      }
    }
  }

  // ignore this for now
  override def importSolution(filename: String): Solution[Rise] = ???

  override def exportSolution(solution: Solution[Rise], filename: String): Unit = ???

}

