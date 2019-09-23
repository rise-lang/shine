package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

final case class RecordAcc(dt1: DataType,
                           dt2: DataType,
                           fst: Phrase[AccType],
                           snd: Phrase[AccType])
  extends AccPrimitive {

  override val t: AccType =
    (dt1: DataType) ->: (dt2: DataType) ->:
      (fst :: acc"[$dt1]") ->: (snd :: acc"[$dt2]") ->:
    acc"[$dt1 x $dt2]"

  override def eval(s: Store): AccIdentifier = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] =
    RecordAcc(fun.data(dt1), fun.data(dt2), VisitAndRebuild(fst, fun), VisitAndRebuild(snd, fun))


  override def xmlPrinter: Elem =
    <recordAcc dt1={ToString(dt1)} dt2={ToString(dt2)}>
      <fst>
        {Phrases.xmlPrinter(fst)}
      </fst>
      <snd>
        {Phrases.xmlPrinter(snd)}
      </snd>
    </recordAcc>

  override def prettyPrint: String =
    s"(${PrettyPhrasePrinter(fst)}, ${PrettyPhrasePrinter(snd)})"
}
