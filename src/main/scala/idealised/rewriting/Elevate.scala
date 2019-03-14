package idealised.rewriting

import idealised.SurfaceLanguage._

object Elevate {

  type Strategy = Expr => Expr

  def isDefined: Strategy => Expr => Boolean = s => e => {
    try { s(e); true } catch { case _: MatchError => false }
  }

  def applyAt: Strategy => Location => Strategy = s => l => expr => {
    l match {
      case Position(pos) =>
        var exprsLeft = pos
        VisitAndRebuild(expr, new VisitAndRebuild.Visitor {
          override def apply(e: Expr): Result = {
            if (exprsLeft == 0) {
              Stop( s(e) )
            } else {
              exprsLeft = exprsLeft - 1
              Continue(e, this)
            }
          }
        })

      case FindFirst(predicate) =>
        VisitAndRebuild(expr, new VisitAndRebuild.Visitor {
          override def apply(e: Expr): Result = {
            if (predicate(e)) {
              Stop( s(e) )
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
