package shine.OpenCL.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class ReduceSeq(unroll: Boolean)
                          (val n: Nat,
                           val initAddrSpace: AddressSpace,
                           val dt1: DataType,
                           val dt2: DataType,
                           val f: Phrase[ExpType ->: ExpType ->: ExpType],
                           val init: Phrase[ExpType],
                           val array: Phrase[ExpType]
                          ) extends ExpPrimitive {
  f :: expT(dt2, read) ->: expT(dt1, read) ->: expT(dt2, write)
  init :: expT(dt2, write)
  array :: expT(n`.`dt1, read)
  override val t: ExpType = expT(dt2, read)
}
