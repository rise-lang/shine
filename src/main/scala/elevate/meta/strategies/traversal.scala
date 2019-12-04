package elevate.meta.strategies

import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.rise.strategies.traversal._
import elevate.meta.strategies.traversal._
import elevate.core.{Failure, Strategy, Success}
import elevate.rise.Rise

object traversal {

  type Elevate = Strategy[Rise]

  // todo can we get replace Lift with P?
  implicit object ElevateTraversable extends elevate.core.strategies.Traversable[Elevate] {
    override def all: Strategy[Elevate] => Strategy[Elevate] = s => ???
    override def some: Strategy[Elevate] => Strategy[Elevate] = s => ???

    override def one: Strategy[Elevate] => Strategy[Elevate] = oneHandlingState(false)
    override def oneUsingState: Strategy[Elevate] => Strategy[Elevate] = oneHandlingState(true)

    def oneHandlingState: Boolean => Strategy[Elevate] => Strategy[Elevate] = carryOverState => s => {
      case seq(first,second) => s(first) match {
        case Success(x: Elevate) => Success(seq(x, second))
        case Failure(state) => if (carryOverState)
          state(second).mapSuccess(seq(first,_)) else
          s(second).mapSuccess(seq(first,_))
      }
      case downup2(p1,p2) => ???

      case all(s) => ???
      case one(s) => ???
      case some(s) => ???
      case oneUsingState(s) => ???

      case body(p) => s(p).mapSuccess(body)
      case function(p) => s(p).mapSuccess(function)
      case argument(p) => s(p).mapSuccess(argument)
      case argumentOf(x,p) => s(p).mapSuccess(argumentOf(x,_))

      case oncetd(p) => s(p).mapSuccess(oncetd[Rise](_))
      case topdown(p) => s(p).mapSuccess(topdown[Rise](_))
      case tryAll(p) => s(p).mapSuccess(tryAll[Rise](_))
      case bottomup(p) => s(p).mapSuccess(bottomup[Rise](_))
      case downup(p) => s(p).mapSuccess(downup[Rise](_))
      case oncebu(p) => s(p).mapSuccess(oncebu[Rise](_))
      case alltd(p) => s(p).mapSuccess(alltd[Rise](_))
      case sometd(p) => s(p).mapSuccess(sometd[Rise](_))
      case somebu(p) => s(p).mapSuccess(somebu[Rise](_))
      case position(i) => ???
      case skip(n) => ???
        // todo rules should fail but everything else should be a warning or something similar
      case x => /*println(s"???: $x");*/Failure(s)
    }
  }

}
