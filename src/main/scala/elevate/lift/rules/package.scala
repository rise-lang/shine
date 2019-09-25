package elevate.lift

import elevate.core.{Failure, Strategy, Success}
import elevate.lift.strategies.predicate._
import lift.core._
import lift.core.types._

package object rules {

  def betaReduction: Strategy = {
    case Apply(f, x) => lifting.liftFunExpr(f) match {
      case lifting.Reducing(lf) => Success(lf(x))
      case _ => Failure(betaReduction)
    }
    case DepApply(f, x: Nat) => lifting.liftDepFunExpr[NatKind](f) match {
      case lifting.Reducing(lf) => Success(lf(x))
      case _ => Failure(betaReduction)
    }
    case _ => Failure(betaReduction)
  }

  def etaReduction: Strategy = {
    case Lambda(x1, Apply(f, x2)) if x1 == x2 && !contains(x1)(f) => Success(f)
    case _ => Failure(etaReduction)
  }

  def etaAbstraction: Strategy = {
    // TODO? check that `f` is a function (i.e. has a function type)
    case f =>
      val x = Identifier(freshName("η"))
      Success(Lambda(x, Apply(f, x)))
  }
}
