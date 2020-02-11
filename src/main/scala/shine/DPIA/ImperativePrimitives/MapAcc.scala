package shine.DPIA.ImperativePrimitives

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.xml.Elem

final case class MapAcc(n: Nat,
                        dt1: DataType,
                        dt2: DataType,
                        f: Phrase[AccType ->: AccType],
                        array: Phrase[AccType])
  extends AccPrimitive {

  f :: accT(dt1) ->: accT(dt2)
  array :: accT(n`.`dt1)
  override val t: AccType = accT(n`.`dt2)

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] = {
    MapAcc(fun.nat(n), fun.data(dt1), fun.data(dt2), VisitAndRebuild(f, fun), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): AccIdentifier = ???

  override def prettyPrint: String = s"(mapAcc ${PrettyPhrasePrinter(f)} ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <mapAcc n={ToString(n)} dt1={ToString(dt1)} dt2={ToString(dt2)}>
      <f type={ToString(AccType(dt1) ->: AccType(dt2))}>
        {Phrases.xmlPrinter(f)}
      </f>
      <input type={ToString(AccType(ArrayType(n, dt1)))}>
        {Phrases.xmlPrinter(array)}
      </input>
    </mapAcc>
}
