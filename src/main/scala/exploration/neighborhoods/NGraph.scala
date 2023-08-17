package exploration.neighborhoods

import elevate.heuristic_search.HeuristicPanel
import elevate.heuristic_search.util.Solution
import elevate.heuristic_search.Runner
import elevate.core._
import elevate.heuristic_search.util._

import rise.elevate.Rise
import exploration.rewriter.everywhere._

/**
  * Neighborhood is defined by graph
  * Warning: Cannot implement this by now as we cannot
  * recognize similar expression created by different
  * rewrite paths
  */
case class NGraph(
                   runner: Runner[Rise],
                   strategies: Seq[Strategy[Rise]],
                   afterRewrite: Option[Strategy[Rise]] = None,
                   checkExpression: Option[Rise => Boolean] = None
                 ) extends HeuristicPanel[Rise] {

  val solutions = new scala.collection.mutable.HashMap[String, Option[Double]]()

  // todo implement this

  // relies on bidirectional rules
  override def N(solution: Solution[Rise]): Seq[Solution[Rise]] = {

    val rewritten: Seq[Solution[Rise]] = strategies.flatMap(rule => {

      // try each possible rule application
      val provisionalRewrites = everywhere(rule).apply(solution.expression())

      // apply after-rewrite if provided
      val rewrites = afterRewrite match {
        case Some(aftermath) =>
          provisionalRewrites
            .map(e => aftermath.apply(e)) // apply aftermath
            .filter(e => e.isInstanceOf[Success[Rise]]) // get successes
            .map(e => e.get) // get expressions from rewrite result
        case None =>
          provisionalRewrites
      }

      // create solutions from rewritten expressions
      val solutions: Seq[Solution[Rise]] = Range(0, rewrites.size).zip(rewrites).map(elem => {

        // create new step
        val step = SolutionStep[Rise](
          expression = elem._2,
          strategy = rule,
          location = elem._1
        )

        // add step to solution
        Solution[Rise](
          solutionSteps = (solution.solutionSteps :+ step).drop(1)
        )
      })
      solutions
    })

    rewritten
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

