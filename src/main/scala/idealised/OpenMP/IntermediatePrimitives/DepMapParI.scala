package idealised.OpenMP.IntermediatePrimitives

import idealised.DPIA.DSL._
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{AccType, CommandType, DataType, ExpType}
import idealised.DPIA._
import idealised.OpenMP.DSL.parForNat

//noinspection TypeAnnotation
object DepMapParI {
  def apply(n: Nat,
            ft1:NatDataTypeFunction,
            ft2:NatDataTypeFunction,
            f: Phrase[`(nat)->`[ExpType -> (AccType -> CommandType)]],
            in: Phrase[ExpType],
            out: Phrase[AccType]): Phrase[CommandType] =
  {
    parForNat(n, ft2.x, ft2.body, out, idx => a => f(idx)(in `@d` idx)(a))
  }
}