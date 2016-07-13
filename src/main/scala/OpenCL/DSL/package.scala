package OpenCL

import _root_.Core._
import _root_.Core.OperationalSemantics.IndexData
import _root_.DSL.typed.{λ, _Λ_}
import apart.arithmetic.NamedVar

package object DSL {

  val reorderWithStridePhrase = {
    _Λ_(s =>
      _Λ_(n => λ(exp"[idx($n)]")(i => {
        val j = NamedVar(i.name)
        LiteralPhrase(IndexData((j / (n /^ s)) + s * (j % (n /^ s))), IndexType(n))
      }))
    )
  }

}
