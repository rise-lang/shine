package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._

final case class DMatch(x: NatIdentifier,
                        elemT: DataType,
                        outT: DataType,
                        a: AccessType,
                        f: Phrase[`(nat)->:`[ExpType ->: ExpType]],
                        input: Phrase[ExpType]
                       ) extends ExpPrimitive {
  override val t: ExpType = expT(outT, a)

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] =
    DMatch(v.nat(x), v.data(elemT), v.data(outT), v.access(a), VisitAndRebuild(f, v), VisitAndRebuild(input, v))
}
