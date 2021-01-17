package shine.DPIA.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class MapRead(n: Nat,
                         dt1: DataType,
                         dt2: DataType,
                         f: Phrase[ExpType ->: (ExpType ->: CommType) ->: CommType],
                         input: Phrase[ExpType]
                        ) extends ExpPrimitive {
  f :: expT(dt1, read) ->: (expT(dt2, read) ->: comm) ->: comm
  input :: expT(n`.`dt1, read)
  override val t: ExpType = expT(n`.`dt2, read)
}
