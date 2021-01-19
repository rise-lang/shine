package shine.DPIA.ImperativePrimitives

import shine.DPIA.Phrases.{AccPrimitive, Phrase, VisitAndRebuild}
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics.Store
import shine.DPIA.Types.{AccType, DataType}
import shine.DPIA.accT

import scala.xml.Elem


final case class TransmuteAcc(inT: DataType, outT:DataType, A:Phrase[AccType]) extends AccPrimitive {
  override val t = accT(inT)


  override def prettyPrint: String = s"transmuteAcc"

  override def xmlPrinter: Elem = <transmuteAcc></transmuteAcc>

  override def eval(s: Store): OperationalSemantics.AccIdentifier = ???

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): TransmuteAcc =
    TransmuteAcc(v.data(inT), v.data(outT), VisitAndRebuild(A, v))
}
