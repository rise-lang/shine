package OpenCL

import _root_.Core._
import _root_.DSL.typed._

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
