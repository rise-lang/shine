package exploration

import elevate.core.Strategy
import elevate.heuristic_search.util.Solution
import rise.autotune
import rise.autotune.wrapOclRun
import rise.core.Expr
import rise.core.types.{Nat, NatIdentifier}
import rise.elevate.Rise
import shine.OpenCL.{GlobalSize, LocalSize}
import util.gen

package object runner {
  def checkSolution(lowering: Strategy[Rise], solution: Solution[Rise]): Boolean = {

    // todo check expression using checking function

    // try to compile here codegen
    // do we need to inject parameters?

    // lower expression
    val e: Expr = solution.expression
    val lowered = lowering.apply(e)
    val loweredOcl: Expr = wrapOclRun(LocalSize(1, 1), GlobalSize(1, 1))(lowered.get)

    // replace each parameter with constant 1
    val params = autotune.constraints.collectParameters(loweredOcl)
    val paramMap:Map[NatIdentifier, Nat] = params.map(param => {
      param -> (1: Nat)
    }).toMap[NatIdentifier, Nat]
    val loweredOclReplaced = rise.core.substitute.natsInExpr(paramMap.toMap[Nat, Nat], loweredOcl)

    // try to generate code
    try {
      gen.opencl.hosted("fun").fromExpr(loweredOclReplaced)
      true
    } catch {
      case e: Throwable =>
        false
    }
  }
}
