package shine.DPIA.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.accPrimitive

@accPrimitive
final case class ReorderAcc(n: Nat,
                            dt: DataType,
                            idxF: Phrase[ExpType ->: ExpType],
                            array: Phrase[AccType]
                           ) extends AccPrimitive {
  idxF :: expT(idx(n), read) ->: expT(idx(n), read)
  array :: accT(n`.`dt)
  override val t: AccType = accT(n`.`dt)
}
