package idealised.SurfaceLanguage.DSL

import idealised.SurfaceLanguage.{->, Expr}
import idealised.SurfaceLanguage.Types.DataType
import lift.arithmetic.ArithExpr

object slide2D {
  def apply(size:ArithExpr, step:ArithExpr):Expr[DataType -> DataType] = {
    fun(xs => xs :>> map(slide(size,step)) :>> slide(size, step) :>> map(transpose))
  }
}

object pad2D {

}
