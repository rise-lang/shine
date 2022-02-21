import elevate.heuristic_search.util.Solution
import rise.elevate.Rise

package object exploration {

  def checkSolution(solution: Solution[Rise]): Boolean = {

//    solution.expression
    // case match stuff?
    // ast?

    // mapGlb(x) only if not mapGlb(x), mapLocal(x), mapWrg(x)
    // mapLcl(x) only inside mapWrg(x)


    // traverse expression and check for nesting of patterns

    true
  }

}
