package shine.DPIA.ImperativePrimitives

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA._

import scala.xml.Elem

final case class PairAcc1(dt1: DataType,
                          dt2: DataType,
                          pair: Phrase[AccType])
  extends AccPrimitive {

  pair :: accT(dt1 x dt2)
  override val t: AccType = accT(dt1)

  override def eval(s: Store): AccIdentifier = {
    OperationalSemantics.eval(s, pair) match {
      case r: PairIdentifier => r.fst
      case _ => throw new Exception("This should not happen")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] =
    PairAcc1(fun.data(dt1), fun.data(dt2), VisitAndRebuild(pair, fun))


  override def xmlPrinter: Elem =
    <recordAcc1 dt1={ToString(dt1)} dt2={ToString(dt2)}>
      {Phrases.xmlPrinter(pair)}
    </recordAcc1>

  override def prettyPrint: String = s"(RecordAcc1 ${PrettyPhrasePrinter(pair)})"
}
