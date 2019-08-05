package idealised.SurfaceLanguage.DSL

import idealised.SurfaceLanguage.Semantics.Data
import idealised.SurfaceLanguage.Types.IndexType
import idealised.SurfaceLanguage.{Expr, LiteralExpr}
import lift.arithmetic.{ArithExpr, SteppedCase}

object slide2D {
  def apply(size: ArithExpr, step: ArithExpr): Expr = {
    fun(xs => xs :>> map(slide(size, step)) :>> slide(size, step) :>> map(transpose))
  }
}

object pad2D {
  /**
    *
    * @param n    The inner dimension of the input array
    * @param l    The amount of left-pad
    * @param r    The amount of right-pad
    * @param data The inlineable data for the pad
    * @return
    */
  def apply(n: ArithExpr, l: ArithExpr, r: ArithExpr, data: Data): Expr = {
    fun(xs => xs :>> pad(l, r, generate(fun(IndexType(n))(_ => LiteralExpr(data)))) :>> map(pad(l, r, LiteralExpr(data))))
  }

}


object partition2D {
  //TODO: Investiage. this might be wrong
  def apply(outerSize: ArithExpr, innerSize: ArithExpr): Expr = {
    map(
      partition(3, m => SteppedCase(m, Seq(outerSize, innerSize, outerSize)))
    ) >>> partition(3, m => SteppedCase(m, Seq(outerSize, innerSize, outerSize)))
  }
}
