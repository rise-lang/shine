package idealised.SurfaceLanguage.DSL

import idealised.DPIA.ImperativePrimitives._
import idealised.DPIA.Types.PhraseType
import idealised.SurfaceLanguage.{Expr, IfThenElseExpr}

object `if` {
  def apply[T <: PhraseType](cond: DataExpr,
                             thenP: Expr[T],
                             elseP: Expr[T]): IfThenElseExpr[T] =
    IfThenElseExpr(cond, thenP, elseP)
}

object skip extends Skip
