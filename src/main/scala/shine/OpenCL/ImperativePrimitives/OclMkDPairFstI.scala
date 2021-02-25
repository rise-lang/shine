package shine.OpenCL.ImperativePrimitives


import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types._
import shine.DPIA.{->:, _}

import scala.xml.Elem


final case class OclMkDPairFstI(fst: NatCollectionIdentifier, body:Phrase[`(nat)->:`[ExpType ->: AccType ->: CommType]], A:Phrase[AccType]) extends CommandPrimitive {
  override val t = comm

  override def eval(s: Store) = ???

  override def prettyPrint: String = s"${this.getClass.getSimpleName} ($fst) (${PrettyPhrasePrinter(body)})"

  override def xmlPrinter: Elem = <MkDPairFstI>
    <fst>
      {ToString(fst)}
    </fst>
    <snd>
      {ToString(body)}
    </snd>
  </MkDPairFstI>

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): OclMkDPairFstI = OclMkDPairFstI(
    f.natCollection(fst),
    VisitAndRebuild(body, f),
    VisitAndRebuild(A, f)
  )
}
