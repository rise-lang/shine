package idealised.SurfaceLanguage.DSL

import idealised.SurfaceLanguage.Semantics.{Data, SingletonArrayData}
import idealised.SurfaceLanguage.{->, Expr, LiteralExpr}
import idealised.SurfaceLanguage.Types.DataType
import lift.arithmetic.ArithExpr

object slide2D {
  def apply(size:ArithExpr, step:ArithExpr):Expr[DataType -> DataType] = {
    fun(xs => xs :>> map(slide(size,step)) :>> slide(size, step) :>> map(transpose))
  }
}

object pad2D {
  /**
    *
    * @param n The inner dimension of the input array
    * @param l The amount of left-pad
    * @param r The amount of right-pad
    * @param data The inlineable data for the pad
    * @return
    */
  def apply(n:ArithExpr, l:ArithExpr, r:ArithExpr, data:Data):Expr[DataType -> DataType] = {
    fun(xs =>  xs :>> pad(l, r, LiteralExpr(SingletonArrayData(n, data))) :>> map(pad(l, r, LiteralExpr(data))) :>> printType("PaddedType"))
  }
}
