package shine.DPIA.primitives.imperative

import shine.DPIA.NatIdentifier
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types.{AccType, DataType}

import scala.xml.Elem

final case class MkDPairSndAcc(fst: NatIdentifier, sndT: DataType, A: Phrase[AccType]) extends AccPrimitive {
  override val t = AccType(sndT)

  override def eval(s: Store): OperationalSemantics.AccIdentifier = ???

  override def prettyPrint: String = s"${this.getClass.getSimpleName} ($fst) (${PrettyPhrasePrinter(A)})"

  override def xmlPrinter: Elem = <MkDPairSndAcc sndT={ToString(sndT)}>
    <fst>
      {ToString(fst)}
    </fst>
    <snd>
      {ToString(A)}
    </snd>
  </MkDPairSndAcc>

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): Phrase[AccType] = MkDPairSndAcc(
    f.nat(fst),
    f.data(sndT),
    VisitAndRebuild(A, f)
  )
}
