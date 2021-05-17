package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._

final case class MakeDepPair(a: AccessType,
                             fst: NatIdentifier,
                             sndT: DataType,
                             snd: Phrase[ExpType]
                        ) extends ExpPrimitive {
  override val t: ExpType = expT(DepPairType(fst, sndT), a)

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] =
    MakeDepPair(v.access(a), v.nat(fst), v.data(sndT), VisitAndRebuild(snd, v))
}
