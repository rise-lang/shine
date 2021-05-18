package shine.DPIA.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._

final case class MkDPairFstI(fst: Nat,
                             A: Phrase[AccType]) extends CommandPrimitive {
  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[CommType] =
    MkDPairFstI(v.nat(fst), VisitAndRebuild(A, v))
}
