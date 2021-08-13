package rise.elevate.meta

import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.core.{Failure, RewriteResult, Strategy, Success}
import rise.elevate.Rise
import rise.elevate.rules.traversal._
import elevate.core.strategies.Traversable
// import rise.elevate.rules.traversal.default._

// implementing elevate.core.strategies.Traversable for Strategy[Rise]

object traversal {

  // implemented just enough to get allow using FNF (see meta.rules.fission)
  implicit class MetaRiseTraversable(ev: Traversable[Rise]) extends Traversable[Strategy[Rise]] {
    override def all: Strategy[Strategy[Rise]] => Strategy[Strategy[Rise]] = s => ???
    override def some: Strategy[Strategy[Rise]] => Strategy[Strategy[Rise]] = s => ???

    override def one: Strategy[Strategy[Rise]] => Strategy[Strategy[Rise]] = oneHandlingState(false)
    override def oneUsingState: Strategy[Strategy[Rise]] => Strategy[Strategy[Rise]] = oneHandlingState(true)

    def oneHandlingState: Boolean => Strategy[Strategy[Rise]] => Strategy[Strategy[Rise]] = carryOverState => s => {
      case Seq(first: Strategy[Rise]@unchecked, second: Strategy[Rise]@unchecked) => s(first) match {
        case Success(x: Strategy[Rise]) => Success(seq(x)(second))
        case Failure(state) => if (carryOverState)
          state(second).mapSuccess(seq(first)(_)) else
          s(second).mapSuccess(seq(first)(_))
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

      case topDown(p) => s(p).mapSuccess(topDown[Rise](_)(ev))
      case allTopdown(p) => s(p).mapSuccess(allTopdown[Rise](_)(ev))
      case tryAll(p) => s(p).mapSuccess(tryAll[Rise](_)(ev))
      case allBottomup(p) => s(p).mapSuccess(allBottomup[Rise](_)(ev))
      case downup(p) => s(p).mapSuccess(downup[Rise](_)(ev))
      case bottomUp(p) => s(p).mapSuccess(bottomUp[Rise](_)(ev))
      case alltd(p) => s(p).mapSuccess(alltd[Rise](_)(ev))
      case sometd(p) => s(p).mapSuccess(sometd[Rise](_)(ev))
      case somebu(p) => s(p).mapSuccess(somebu[Rise](_)(ev))
      case position(i) => ???
      case skip(n) => ???

      // todo rules should fail but everything else should be a warning
      case x => /*println(s"???: $x");*/Failure(s)
    }
  }

  // Meta-Rise-specific Traversals

  case class inBody(s: Strategy[Strategy[Rise]]) extends Strategy[Strategy[Rise]] {
    def apply(e: Strategy[Rise]): RewriteResult[Strategy[Rise]] = e match {
      case body(x: Strategy[Rise]) => s(x).mapSuccess(body)
      case _ => Failure(s)
    }
  }
}
