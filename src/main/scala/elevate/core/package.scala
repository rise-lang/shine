package elevate

import lift.core._

package object core {
  type Strategy = Expr => Expr

  case class LocationNotFound(l: Location, e: Expr) extends Exception

  def isDefined: Strategy => Expr => Boolean = s => e => {
    try { s(e); true }
    catch {
      case _: MatchError => false
      case _: LocationNotFound => false
    }
  }

  def applyAt: Strategy => Location => Strategy = s => l => expr => {
    import traversal.{Result, Stop, Continue}

    val predicate = l match {
      case Position(pos) => {
        var remaining: Int = pos

        { _: Expr =>
          if (remaining == 0) { true }
          else { remaining -= 1; false }
        }
      }
      case FindFirst(pred) => pred
    }

    val (found, result) =
      traversal.DepthFirstGlobalStop(expr,
        new traversal.Visitor {
          override def apply(e: Expr): Result[Expr] = {
            if (predicate(e)) {
              Stop( s(e) )
            } else {
              Continue(e, this)
            }
          }})

    if (!found) {
      throw new LocationNotFound(l, expr)
    }

    result
  }

  implicit class Then(f: Strategy) {
    def `;`(s: Strategy): Strategy = strategies.seq(f)(s)
  }

  implicit class LeftChoice(f: Strategy) {
    def +>(s: Strategy): Strategy = strategies.leftChoice(f)(s)
  }
}
