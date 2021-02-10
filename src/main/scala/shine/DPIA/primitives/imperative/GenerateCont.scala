package shine.DPIA.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

// note: would not be necessary if generate was defined as indices + map
@expPrimitive
final case class GenerateCont(n: Nat,
                              dt: DataType,
                              f: Phrase[ExpType ->: ((ExpType ->: CommType) ->: CommType)]
                             ) extends ExpPrimitive {
  f :: expT(idx(n), read) ->: (expT(dt, read) ->: comm) ->: comm
  override val t: ExpType = expT(n`.`dt, read)
}
