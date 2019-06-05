package elevate.core

import lift.core._

package object rules {
  def betaReduction: Strategy = {
    case Apply(f, x) => lifting.liftFunExpr(f) match {
      case lifting.Reducing(lf) => lf(x)
    }
  }

  def etaReduction: Strategy = {
    // TODO? 'x' should not be used in 'f'
    case Lambda(x1, Apply(f, x2)) if x1 == x2 => f
  }

  def etaAbstraction: Strategy = {
    // TODO? what if this is not a function
    case f =>
      val x = Identifier(freshName("η"))
      Lambda(x, Apply(f, x))
  }
}
