package shine.OpenMP.primitives.intermediate

import shine.DPIA.DSL._
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenMP.DSL.parForNat

//noinspection TypeAnnotation
object DepMapParI {
  def apply(n: Nat,
            ft1: NatToData,
            ft2: NatToData,
            f: Phrase[`(nat)->:`[ExpType ->: AccType ->: CommType]],
            in: Phrase[ExpType],
            out: Phrase[AccType]): Phrase[CommType] = {
    parForNat(n, ft2, out, idx => a => f(idx)(in `@d` idx)(a))
  }
}
