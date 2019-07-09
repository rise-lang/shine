package elevate.core

import lift.core._

package object rules {

  def Rule: Strategy => Strategy = s => e => try s(e) catch {
    case _: MatchError => throw NotApplicable(s)
  }

  def betaReduction: Strategy = Rule({
    case Apply(f, x) => lifting.liftFunctionExpr(f) match {
      case lifting.Reducing(lf) => lf(x)
    }
  })

  def etaReduction: Strategy = Rule({
    // TODO? 'x' should not be used in 'f'
    case Lambda(x1, Apply(f, x2)) if x1 == x2 => f
  })

  def etaAbstraction: Strategy = Rule({
    // TODO? what if this is not a function
    case f =>
      val x = Identifier(freshName("η"))
      Lambda(x, Apply(f, x))
  })
}
