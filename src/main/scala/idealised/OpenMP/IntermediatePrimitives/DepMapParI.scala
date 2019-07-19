package idealised.OpenMP.IntermediatePrimitives

import idealised.DPIA.DSL._
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenMP.DSL.parForNat

//noinspection TypeAnnotation
object DepMapParI {
  def apply(n: Nat,
            ft1:NatToData,
            ft2:NatToData,
            f: Phrase[`(nat)->`[ExpType ->: AccType ->: CommType]],
            in: Phrase[ExpType],
            out: Phrase[AccType]): Phrase[CommType] =
  {
    parForNat(n, ft2, out, idx => a => f(idx)(in `@d` idx)(a))
  }
}