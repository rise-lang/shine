package idealised.OpenCL

import idealised.Core._
import idealised.DSL.typed._

package object DSL {

  val reorderWithStridePhrase = {
    _Λ_((s: Nat) =>
      _Λ_((n: Nat) => λ(exp"[idx($n)]")(i => {
        val j = i asNatIdentifier (withUpperBound = n)

        val newIndex = (j / (n /^ s)) + s * (j % (n /^ s))

        newIndex asPhrase (withType = IndexType(n))
      }))
    )
  }

}
