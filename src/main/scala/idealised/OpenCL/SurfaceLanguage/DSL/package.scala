package idealised.OpenCL.SurfaceLanguage

import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.Types._

package object DSL {
  val reorderWithStride: Expr[`(nat)->`[DataType -> DataType]] = {
    nFun(s => {
      val f =
        nFun(n =>
          fun(IndexType(n))(i => {
            unsafeAsIndex(n, fmapExprNat(asNat(i), j =>
              (j / (n /^ s)) + s * (j % (n /^ s))))
          }))
      reorder(f, f)
    })
  }
}
