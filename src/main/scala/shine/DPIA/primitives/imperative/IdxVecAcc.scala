package shine.DPIA.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.accPrimitive

@accPrimitive
final case class IdxVecAcc(n: Nat,
                           st: DataType,
                           index: Phrase[ExpType],
                           vector: Phrase[AccType]
                          ) extends AccPrimitive {
  index :: expT(idx(n), read)
  vector :: accT(vec(n, st))
  override val t: AccType = accT(st)
}
