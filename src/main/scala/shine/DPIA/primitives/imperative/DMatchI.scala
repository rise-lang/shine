package shine.DPIA.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._

final case class DMatchI(x: NatIdentifier,
                         elemT: DataType,
                         outT: DataType,
                         f: Phrase[`(nat)->:`[ExpType ->: CommType]],
                         input: Phrase[ExpType]
                        ) extends CommandPrimitive {
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[CommType] =
    DMatchI(v.nat(x), v.data(elemT), v.data(outT), VisitAndRebuild(f, v), VisitAndRebuild(input, v))
}
