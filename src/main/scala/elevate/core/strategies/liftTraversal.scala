package elevate.core.strategies

import elevate.core.{Failure, Strategy, Success}
import lift.core.{Apply, Lambda}

object liftTraversal {

  def body: Strategy => Strategy =
    s => {
      case Lambda(x, f) => s(f).mapSuccess(Lambda(x, _))
      case _ => Failure(s)
    }

  def function: Strategy => Strategy =
    s => {
      case Apply(f, e) => s(f).mapSuccess(Apply(_, e))
      case _ => Failure(s)
    }

  def argument: Strategy => Strategy =
    s => {
      case Apply(f, e) => s(e).mapSuccess(Apply(f, _))
      case x => Failure(s)
    }
}
