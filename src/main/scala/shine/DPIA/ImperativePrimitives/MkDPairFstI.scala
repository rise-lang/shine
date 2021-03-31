package shine.DPIA.ImperativePrimitives

import shine.DPIA.Phrases.VisitAndRebuild.KindVisitable
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types._

import scala.xml.Elem


final case class MkDPairFstI[K <: Kind:KindVisitable](fst: K#T, A: Phrase[AccType]) extends CommandPrimitive {
  override val t = comm

  override def eval(s: Store) = ???

  override def prettyPrint: String = s"${this.getClass.getSimpleName} ($fst) (${PrettyPhrasePrinter(A)})"

  override def xmlPrinter: Elem = <MkDPairFstI>
    <fst>
      {ToString(fst)}
    </fst>
    <snd>
      {ToString(A)}
    </snd>
  </MkDPairFstI>

  override def visitAndRebuild(f: VisitAndRebuild.Visitor): MkDPairFstI[K] = MkDPairFstI[K](
    implicitly[KindVisitable[K]].visit(f, fst),
    VisitAndRebuild(A, f)
  )
}
