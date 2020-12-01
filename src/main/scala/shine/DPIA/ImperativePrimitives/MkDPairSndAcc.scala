package shine.DPIA.ImperativePrimitives

import shine.DPIA.Phrases.VisitAndRebuild.KindVisitable
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types.{AccType, DataType, Kind}

import scala.xml.Elem

final case class MkDPairSndAcc[K <: Kind:KindVisitable](fst: K#I, sndT: DataType, A: Phrase[AccType]) extends AccPrimitive {
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
    implicitly[KindVisitable[K]].visit(f, fst),
    f.data(sndT),
    VisitAndRebuild(A, f)
  )
}
