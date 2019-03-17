package idealised.OpenCL.SurfaceLanguage

import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.Types._

package object DSL {
  val reorderWithStride: Expr = {
    nFun(s => {
      val f =
        nFun(n =>
          fun(IndexType(n))(i => {
            fmapIndexExpr(i, j => (j / (n /^ s)) + s * (j % (n /^ s)))
          }))
      reorder(f, f)
    })
  }
}
