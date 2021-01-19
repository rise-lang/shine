package shine.DPIA.primitives.imperative

import shine.DPIA.NatIdentifier
import shine.DPIA.Phrases._
import shine.DPIA.Types.{AccType, DataType}
import shine.macros.Primitive.accPrimitive

@accPrimitive
final case class MkDPairSndAcc(fst: NatIdentifier,
                               sndT: DataType,
                               A: Phrase[AccType]) extends AccPrimitive {
  override val t = AccType(sndT)
}
