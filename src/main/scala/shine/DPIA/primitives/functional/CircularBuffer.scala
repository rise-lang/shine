package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class CircularBuffer(n: Nat,
                                alloc: Nat,
                                sz: Nat,
                                dt1: DataType,
                                dt2: DataType,
                                load: Phrase[ExpType ->: ExpType],
                                input: Phrase[ExpType]
                               ) extends ExpPrimitive {
  load :: expT(dt1, read) ->: expT(dt2, write)
  input :: expT((n - 1 + sz)`.`dt1, read)
  override val t: ExpType = expT(n`.`(sz`.`dt2), read)
}
