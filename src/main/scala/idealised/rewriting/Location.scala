package idealised.rewriting

import idealised.SurfaceLanguage.Types.Type
import idealised.SurfaceLanguage.{Expr, VisitAndRebuild}
import idealised.rewriting.Elevate.LiftExpr

sealed trait Location

final case class Position(pos: Int) extends Location

final case class FindFirst(predicate: LiftExpr => Boolean) extends Location

object Location {

  def findAll: (LiftExpr => Boolean) => LiftExpr => Seq[Location] = predicate => expr => {
    var locations = Seq[Location]()
    var pos = 0
    VisitAndRebuild(expr, new VisitAndRebuild.Visitor {
      override def apply[T <: Type](e: Expr[T]): Result[Expr[T]] = {
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
