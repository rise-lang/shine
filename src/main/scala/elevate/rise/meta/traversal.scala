package elevate.rise.meta

import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import elevate.core.{Failure, MetaStrategy, RewriteResult, Strategy, Success}
import elevate.rise.Rise
import elevate.rise.rules.traversal._

// implementing elevate.core.strategies.Traversable for Strategy[Rise]

object traversal {

  // implemented just enough to get allow using FNF (see meta.rules.fission)
  implicit object MetaRiseTraversable extends elevate.core.strategies.Traversable[Strategy[Rise]] {
    override def all: Strategy[Strategy[Rise]] => Strategy[Strategy[Rise]] = s => ???
    override def some: Strategy[Strategy[Rise]] => Strategy[Strategy[Rise]] = s => ???

    override def one: Strategy[Strategy[Rise]] => Strategy[Strategy[Rise]] = oneHandlingState(false)
    override def oneUsingState: Strategy[Strategy[Rise]] => Strategy[Strategy[Rise]] = oneHandlingState(true)

    def oneHandlingState: Boolean => Strategy[Strategy[Rise]] => Strategy[Strategy[Rise]] = carryOverState => s => {
      case seq(first,second) => s(first) match {
        case Success(x: Strategy[Rise]) => Success(seq(x, second))
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

      // todo rules should fail but everything else should be a warning
      case x => /*println(s"???: $x");*/Failure(s)
    }
  }

  // Meta-Rise-specific Traversals

  case class inBody(s: MetaStrategy[Rise]) extends MetaStrategy[Rise] {
    def apply(e: Strategy[Rise]): RewriteResult[Strategy[Rise]] = e match {
      case body(x: Strategy[Rise]) => s(x).mapSuccess(body)
      case _ => Failure(s)
    }
  }
}
