package idealised.rewriting

import idealised.SurfaceLanguage.{Expr, VisitAndRebuild}

sealed trait Location

final case class Position(pos: Int) extends Location

final case class FindFirst(predicate: Expr => Boolean) extends Location

object Location {

  def findAll: (Expr => Boolean) => Expr => Seq[Location] = predicate => expr => {
    var locations = Seq[Location]()
    var pos = 0
    VisitAndRebuild(expr, new VisitAndRebuild.Visitor {
      override def apply(e: Expr): Result = {
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
