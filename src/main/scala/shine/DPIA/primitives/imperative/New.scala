package shine.DPIA.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.comPrimitive

@comPrimitive
final case class New(dt: DataType,
                     f: Phrase[VarType ->: CommType]
                    ) extends CommandPrimitive {
  f :: varT(dt) ->: comm
}
