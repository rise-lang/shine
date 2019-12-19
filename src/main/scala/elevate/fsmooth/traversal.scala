package elevate.fsmooth

import elevate.core._
import elevate.core.strategies.predicate._
import FSmooth._

object traversal {

  implicit object FSmoothTraversable extends elevate.core.strategies.Traversable[FSmooth] {

    override def all: Strategy[FSmooth] => Strategy[FSmooth] =  s => {
      case Abstraction(i, body, t) => s(body).mapSuccess(Abstraction(i, _, t))
      case i:Variable => Success(i)

      case Application(f, args, t) =>
        val allRewritten = s(f) +: args.map(s(_))
        if (allRewritten.forall(rewriteResultToBoolean))
          Success(Application(s(f).get, args.map(s(_).get)))
        else
          Failure(s)

      case Let(x, value, body, t) =>
        (s(value), s(body)) match {
          case (Success(v), Success(b)) => Success(Let(x, v, b))
          case _ => Failure(s)
        }

      case Conditional(cond, thenBranch, elseBranch, t) =>
        (s(cond), s(thenBranch), s(elseBranch)) match {
          case (Success(c), Success(t), Success(e)) => Success(Conditional(c,t,e))
          case _ => Failure(s)
        }

      case s:ScalarValue => Success(s)
      case i:IndexValue => Success(i)
      case c:CardinalityValue => Success(c)
      case c:Constants => Success(c)
    }

    override def one: Strategy[FSmooth] => Strategy[FSmooth] = oneHandlingState(false)
    override def oneUsingState: Strategy[FSmooth] => Strategy[FSmooth] = oneHandlingState(true)

    private def oneHandlingState: Boolean => Strategy[FSmooth] => Strategy[FSmooth] = carryOverState => s => {
      case Abstraction(i, body, t) => s(body).mapSuccess(Abstraction(i, _, t))
      case _:Variable => Failure(s)

      case Application(f, args, t) => s(f) match {
        case Success(f: FSmooth) => Success(Application(s(f).get, args, t))
        case Failure(state) => {
          val strategy = if(carryOverState) state else s
          args.foldLeft[(Boolean, RewriteResult[FSmooth])](true, Failure(s))(
            (state,expr) => {
              val (cont, result) = (state._1, state._2)
              if (cont) strategy(expr) match {
                case Success(rewrittenExpr) =>
                  val newArgs = args.foldLeft[Seq[Expr]](Seq())((list, curr) => {
                    val newExpr = if (curr == expr) rewrittenExpr else curr
                    list :+ newExpr
                  })
                  (false,Success(Application(f, newArgs)))
                case f:Failure[FSmooth] => (true,f)
              } else (cont, result)
            }
          )
        }._2
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

      case _:ScalarValue => Failure(s)
      case _:IndexValue => Failure(s)
      case _:CardinalityValue => Failure(s)
      case _:Constants => Failure(s)
    }

    override def some: Strategy[FSmooth] => Strategy[FSmooth] = ???
  }
}
