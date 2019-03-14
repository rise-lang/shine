package idealised.rewriting

import idealised.SurfaceLanguage.Types.Type
import idealised.SurfaceLanguage._

object Elevate {

  type LiftExpr = Expr[_ <: Type]

  type Strategy = LiftExpr => LiftExpr

  def isDefined: Strategy => LiftExpr => Boolean = s => e => {
    try { s(e); true } catch { case _: MatchError => false }
  }

  def applyAt: Strategy => Location => Strategy = s => l => expr => {
    l match {
      case Position(pos) =>
        var exprsLeft = pos
        VisitAndRebuild(expr, new VisitAndRebuild.Visitor {
          override def apply[T <: Type](e: Expr[T]): Result[Expr[T]] = {
            if (exprsLeft == 0) {
              Stop( s(e).asInstanceOf[Expr[T]] )
            } else {
              exprsLeft = exprsLeft - 1
              Continue(e, this)
            }
          }
        })

      case FindFirst(predicate) =>
        VisitAndRebuild(expr, new VisitAndRebuild.Visitor {
          override def apply[T <: Type](e: Expr[T]): Result[Expr[T]] = {
            if (predicate(e)) {
              Stop( s(e).asInstanceOf[Expr[T]] )
            } else {
              Continue(e, this)
            }
          }
        })
    }
  }

  implicit class Then(f: Strategy) {
    def `;`(s: Strategy): Strategy = Strategies.seq(f)(s)
  }

  implicit class LeftChoice(f: Strategy) {
    def +>(s: Strategy): Strategy = Strategies.leftChoice(f)(s)
  }

}
