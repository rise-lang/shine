package exploration.search

import elevate.heuristic_search.ProblemConstraints
import elevate.rise.Rise

//simple mockup search class
class MockupSearch extends ProblemConstraints[Rise] {

  def N(solution:Rise):Set[Rise] = {
    //add core here
    Set.apply(solution)
  }

  def f(solution:Rise):Double= {
    //add core here
    12
  }

}

