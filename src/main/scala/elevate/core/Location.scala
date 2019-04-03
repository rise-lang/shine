package elevate.core

import lift.core._

sealed trait Location

final case class Position(pos: Int) extends Location

final case class FindFirst(predicate: Expr => Boolean) extends Location

object Location {
  def findAll: (Expr => Boolean) => Expr => Seq[Location] = predicate => expr => {
    var locations = Seq[Location]()
    var pos = 0
    traversal.DepthFirstLocalStop(expr, new traversal.Visitor {
      import traversal.{Result, Continue}

      override def apply(e: Expr): Result[Expr] = {
        if (predicate(e)) {
          locations = locations :+ Position(pos)
          pos = pos + 1
        }
        Continue(e, this)
      }
    })
    locations
  }
}