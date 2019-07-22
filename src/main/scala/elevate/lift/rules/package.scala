package elevate.lift

import elevate.core.{Failure, Strategy, Success}
import lift.core._

package object rules {

  def betaReduction: Strategy = {
    case Apply(f, x) => lifting.liftFunctionExpr(f) match {
      case lifting.Reducing(lf) => Success(lf(x))
      case _ => Failure(betaReduction)
    }
    case _ => Failure(betaReduction)
  }

  def etaReduction: Strategy = {
    // TODO? 'x' should not be used in 'f'
    case Lambda(x1, Apply(f, x2)) if x1 == x2 => Success(f)
    case _ => Failure(etaReduction)
  }

  def etaAbstraction: Strategy = {
    // TODO? what if this is not a function
    case f  =>
      val x = Identifier(freshName("η"))
      Success(Lambda(x, Apply(f, x)))
    case _ => Failure(etaAbstraction)
  }
}
