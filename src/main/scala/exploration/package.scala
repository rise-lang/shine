import elevate.heuristic_search.util.Solution
import rise.elevate.Rise

package object exploration {

  // todo delete this?  or call this from all the executors?
  // runners
  // panels?
  def checkSolution(solution: Solution[Rise]): Boolean = {

    // try catch lowering -> try codegen and execution for this program
    // apply lowering

    // collect errors -> save them

//    solution.expression
    // case match stuff?
    // ast?

    // mapGlb(x) only if not mapGlb(x), mapLocal(x), mapWrg(x)
    // mapLcl(x) only inside mapWrg(x)


    // traverse expression and check for nesting of patterns

    true
  }

}
