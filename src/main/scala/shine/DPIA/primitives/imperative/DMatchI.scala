package shine.DPIA.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.comPrimitive

@comPrimitive
final case class DMatchI(x: NatIdentifier,
                         elemT: DataType,
                         outT: DataType,
                         f: Phrase[`(nat)->:`[ExpType ->: CommType]],
                         input: Phrase[ExpType]
                        ) extends CommandPrimitive {
}
