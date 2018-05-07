package idealised.OpenCL.SurfaceLanguage

import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.Types._

package object DSL {

  val reorderWithStridePhrase: Expr[`(nat)->`[`(nat)->`[DataType -> DataType]]] = {
    dFun((s: Nat) =>
      dFun((n: Nat) =>
        fun(IndexType(n))(i => {
          val j = i asNatIdentifier (withUpperBound = n)
          (j / (n /^ s)) + s * (j % (n /^ s)) asExpr (withType = IndexType(n))
        } ) ) )
  }

}
