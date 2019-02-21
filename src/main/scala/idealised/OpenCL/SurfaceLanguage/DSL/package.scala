package idealised.OpenCL.SurfaceLanguage

import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.Types._

package object DSL {

  val reorderWithStridePhrase: Expr[`(nat)->`[`(nat)->`[DataType -> DataType]]] = {
    //TODO does changing from Nat to NatIdentifier make sense?
    dFun((s: NatIdentifier) =>
      dFun((n: NatIdentifier) =>
        fun(IndexType(n))(i => {
          val j = i asNatIdentifier (withUpperBound = n)
          (j / (n /^ s)) + s * (j % (n /^ s)) asExpr (withType = IndexType(n))
        } ) ) )
  }

}
