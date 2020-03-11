package exploration.search

import elevate.core.strategies.traversal.{alltd, bottomup, oncebu, oncetd, topdown}
import elevate.core.{Failure, Strategy, Success}
import elevate.heuristic_search.ProblemConstraints
import elevate.rise.Rise
import elevate.rise.rules.traversal.LiftTraversable
import elevate.rise.strategies.normalForm.LCNF

// simple mockup search class
class MockupSearch(val runner:Rise => Option[Double], val strategies:Set[Strategy[Rise]]) extends ProblemConstraints[Rise] {

  val solutions = new scala.collection.mutable.HashMap[Int, Option[Double]]()

  def N(solution:Rise):Set[Rise] = {
    val neighbours = scala.collection.mutable.Set[Rise]()

    // try each strategy and add result to neighbourhood set
    strategies.foreach(strategy  => {
      try {
        // apply strategy
        val result = (LCNF `;` oncetd(strategy) `;` LCNF).apply(solution)

        // check rewriting result and it add to neighbourhood set
        result match {
          case _:Success[Rise] => neighbours.add(result.get) //add to neighbourhood
          case _:Failure[Rise] => //nothing
        }
      }catch{
        case e:Throwable => {
          print("rewriting error: " + e +  "\n")
        }
      }
    })

    // add id to neighbourhood
    neighbours.add(solution)

    neighbours.toSet
  }

  // warning: check size of hashmap
  def f(solution:Rise): Option[Double] = {
    // buffer performance values in hashmap
    val test = solutions.get(solution.hashCode())
    solutions.get(solution.hashCode()) match {
      case Some(value) => solutions.get(solution.hashCode()).get
      case _ => {
        val performanceValue = runner(solution)
        solutions.+=(solution.hashCode() -> performanceValue)
        performanceValue
      }
    }
  }
}

