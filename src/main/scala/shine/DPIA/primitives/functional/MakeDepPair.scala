package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class MakeDepPair(a: AccessType,
                             fst: NatIdentifier,
                             sndT: DataType,
                             snd: Phrase[ExpType]
                        ) extends ExpPrimitive {
  override val t: ExpType = expT(DepPairType(fst, sndT), a)
}
