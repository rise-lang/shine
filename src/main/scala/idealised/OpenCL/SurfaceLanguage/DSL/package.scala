package idealised.OpenCL.SurfaceLanguage

import idealised.DPIA.Types.{ExpType, IndexType}
import idealised.DPIA._
import idealised.SurfaceLanguage.DSL.{λ, _}
import idealised.SurfaceLanguage.NatDependentLambdaExpr

package object DSL {

  val reorderWithStridePhrase: NatDependentLambdaExpr[`(nat)->`[ExpType -> ExpType]] = {
    _Λ_((s: Nat) =>
      _Λ_((n: Nat) =>
        λ(exp"[idx($n)]")(i => {
          val j = i asNatIdentifier (withUpperBound = n)
          val newIndex = (j / (n /^ s)) + s * (j % (n /^ s))
          newIndex asExpr (withType = IndexType(n))
        } ) ) )
  }

}
