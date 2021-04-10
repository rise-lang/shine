package shine.OpenMP.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class ReducePar(n: Nat,
                           dt1: DataType, dt2: DataType,
                           f: Phrase[ExpType ->: ExpType ->: ExpType],
                           init: Phrase[ExpType],
                           array: Phrase[ExpType]
                          ) extends ExpPrimitive {
  f :: expT(dt2, read) ->: expT(dt1, read) ->: expT(dt2, write)
  init :: expT(dt2, write)
  array :: expT(n`.`dt1, read)
  override val t: ExpType = expT(dt2, read)
}
