package elevate.fsmooth

import elevate.core.{FSmooth, Failure, RewriteResult, Strategy, Success}
import _root_.FSmooth._

object traversal {

  implicit object FSmoothTraversable extends elevate.core.strategies.Traversable[FSmooth] {
    // todo finish
    override def all: Strategy[FSmooth] => Strategy[FSmooth] =  s => {
      case Abstraction(i, body, t) => s(body).mapSuccess(Abstraction(i, _, t))
      case i:Variable => Success(i)
      case Application(f, args, t) => ???
      case Let(x, value, body, t) => ???
      case Conditional(cond, thenBranch, elseBranch, t) => ???
      case s:ScalarValue => Success(s)
      case i:IndexValue => Success(i)
      case c:CardinalityValue => Success(c)
      case c:Constants => Success(c)
    }

    // todo finish
    override def oneHandlingState: Boolean => Strategy[FSmooth] => Strategy[FSmooth] = carryOverState => s => {
      case Abstraction(i, body, t) => s(body).mapSuccess(Abstraction(i, _, t))
      case i:Variable => Success(i)
      case Application(f, args, t) => s(f) match {
        case Success(f: FSmooth) => ??? // Success(Application(_, args))
        case Failure(state) => {
          val strategy = if(carryOverState) state else s
          val test = args.zip(args).map(x => (strategy(x._1), x._2))
          ???
        }
      }
      case Let(x, value, body, t) => s(value) match {
        case Success(f: FSmooth) => Success(Let(x, f, body, t))
        case Failure(state) => if (carryOverState)
          state(body).mapSuccess(Let(x, value, _, t)) else
          s(body).mapSuccess(Let(x, value, _, t))
      }
      case Conditional(cond, thenBranch, elseBranch, t) => s(cond) match {
        case Success(f: FSmooth) => Success(Conditional(f, thenBranch, elseBranch))
        case Failure(state) => if (carryOverState)
          state(thenBranch).mapSuccess(Conditional(cond, _, elseBranch, t)) else
          s(thenBranch).mapSuccess(Conditional(cond, _, elseBranch, t)) match {
            case s:Success[FSmooth] => s
            case Failure(x) => if (carryOverState)
              x(elseBranch).mapSuccess(Conditional(cond, thenBranch, _, t)) else
              s(elseBranch).mapSuccess(Conditional(cond, thenBranch, _, t))
          }
      }
      case s:ScalarValue => Success(s)
      case i:IndexValue => Success(i)
      case c:CardinalityValue => Success(c)
      case c:Constants => Success(c)
    }
    override def some: Strategy[FSmooth] => Strategy[FSmooth] = ???
  }
}
