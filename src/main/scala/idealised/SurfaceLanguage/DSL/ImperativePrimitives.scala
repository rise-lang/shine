package idealised.SurfaceLanguage.DSL

import idealised.SurfaceLanguage.Types.Type
import idealised.SurfaceLanguage.{Expr, IfThenElseExpr}

object `if` {
  def apply[T <: Type](cond: DataExpr,
                       thenP: Expr[T],
                       elseP: Expr[T]): IfThenElseExpr[T] =
    IfThenElseExpr(cond, thenP, elseP)
}
