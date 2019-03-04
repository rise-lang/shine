package idealised.OpenCL.SurfaceLanguage

import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.Types._

package object DSL {
  val reorderWithStride: Expr[`(nat)->`[DataType -> DataType]] = {
    dFun((s: Nat) => {
      val f =
        dFun((n: Nat) =>
          fun(IndexType(n))(i => {
            val j = i asNatIdentifier (withUpperBound = n)
            (j / (n /^ s)) + s * (j % (n /^ s)) asExpr (withType = IndexType(n))
          }))
      reorder(f, f)
    })
  }
}
