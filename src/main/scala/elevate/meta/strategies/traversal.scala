package elevate.meta.strategies

import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.lift.strategies.traversal._
import elevate.meta.strategies.traversal._
import elevate.core.{Elevate, Failure, Lift, Meta, Strategy, Success}

object traversal {
  // todo can we get replace Lift with P?
  implicit object ElevateTraversable extends elevate.core.strategies.Traversable[Elevate] {
    override def all: Strategy[Elevate] => Strategy[Elevate] = s => ???
    override def some: Strategy[Elevate] => Strategy[Elevate] = s => ???

    override def oneHandlingState: Boolean => Strategy[Elevate] => Strategy[Elevate] = carryOverState => s => {
      case seq(first,second) => s(first) match {
        case Success(x: Elevate) => Success(seq(x, second))
        case Failure(state) => if (carryOverState)
          state(second).mapSuccess(seq(first,_)) else
          s(second).mapSuccess(seq(first,_))
      }
      case downup2(p1,p2) => ???

      case all(s) => ???
      case oneHandlingState(state, s) => ???
      case some(s) => ???

      case body(p) => s(p).mapSuccess(body)
      case function(p) => s(p).mapSuccess(function)
      case inTyped(p) => s(p).mapSuccess(inTyped)
      case argument(p) => s(p).mapSuccess(argument)
      case argumentOf(x,p) => s(p).mapSuccess(argumentOf(x,_))

      case oncetd(p) => s(p).mapSuccess(oncetd[Lift](_))
      case topdown(p) => s(p).mapSuccess(topdown[Lift](_))
      case tryAll(p) => s(p).mapSuccess(tryAll[Lift](_))
      case bottomup(p) => s(p).mapSuccess(bottomup[Lift](_))
      case downup(p) => s(p).mapSuccess(downup[Lift](_))
      case oncebu(p) => s(p).mapSuccess(oncebu[Lift](_))
      case alltd(p) => s(p).mapSuccess(alltd[Lift](_))
      case sometd(p) => s(p).mapSuccess(sometd[Lift](_))
      case somebu(p) => s(p).mapSuccess(somebu[Lift](_))
      case position(i) => ???
      case skip(n) => ???
        // todo rules should fail but everything else should be a warning or something similar
      case x => /*println(s"???: $x");*/Failure(s)
    }
  }

}
