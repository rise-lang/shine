package shine.DPIA.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.comPrimitive

@comPrimitive
final case class ForVec(n: Nat,
                        dt: DataType,
                        out: Phrase[AccType],
                        loopBody: Phrase[ExpType ->: AccType ->: CommType]
                       ) extends CommandPrimitive {
  out :: accT(vec(n, dt))
  loopBody :: expT(idx(n), read) ->: accT(dt) ->: comm
}
