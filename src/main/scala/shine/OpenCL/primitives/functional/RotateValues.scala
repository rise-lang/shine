package shine.OpenCL.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class RotateValues(a: AddressSpace,
                              n: Nat,
                              sz: Nat,
                              dt: DataType,
                              write: Phrase[ExpType ->: ExpType],
                              input: Phrase[ExpType]
                             ) extends ExpPrimitive {
  write :: expT(dt, read) ->: expT(dt, shine.DPIA.Types.write)
  input :: expT((n - 1 + sz)`.`dt, read)
  override val t: ExpType = expT(n`.`(sz`.`dt), read)
}