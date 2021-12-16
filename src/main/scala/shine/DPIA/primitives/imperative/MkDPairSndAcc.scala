package shine.DPIA.primitives.imperative

import rise.core.types.DataType
import shine.DPIA.NatIdentifier
import shine.DPIA.Phrases._
import shine.DPIA.Types.AccType

final case class MkDPairSndAcc(fst: NatIdentifier,
                               sndT: DataType,
                               A: Phrase[AccType]) extends AccPrimitive {
  override val t: AccType = AccType(sndT)

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[AccType] =
    MkDPairSndAcc(v.nat(fst), v.data(sndT), VisitAndRebuild(A, v))
}
