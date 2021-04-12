package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Map(n: Nat,
                     dt1: DataType,
                     dt2: DataType,
                     access: AccessType,
                     f: Phrase[ExpType ->: ExpType],
                     array: Phrase[ExpType]
                    ) extends ExpPrimitive {
  array :: expT(n`.`dt1, access)
  f :: expT(dt1, access) ->: expT(dt2, access)
  override val t: ExpType = expT(n`.`dt2, access)
}
