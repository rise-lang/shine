package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Generate(n: Nat,
                          dt: DataType,
                          f: Phrase[ExpType ->: ExpType]
                         ) extends ExpPrimitive {
  f :: expT(idx(n), read) ->: expT(dt, read)
  override val t: ExpType = expT(n`.`dt, read)
}
