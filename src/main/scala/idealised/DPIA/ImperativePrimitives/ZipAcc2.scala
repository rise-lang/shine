package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types.{AccType, DataType}
import idealised.DPIA._

import scala.xml.Elem

final case class ZipAcc2(n: Nat,
                         dt1: DataType,
                         dt2: DataType,
                         array: Phrase[AccType])
  extends AccPrimitive {

  override lazy val `type`: AccType = acc"[$n.$dt2]"

  override def typeCheck(): Unit = {
    import idealised.DPIA.Types.TypeChecker._
    (n: Nat) -> (dt1: DataType) -> (dt2: DataType) ->
      (array :: acc"[$n.($dt1 x $dt2)]") -> `type`
  }

  override def eval(s: Store): AccIdentifier = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] =
    ZipAcc2(fun(n), fun(dt1), fun(dt2), VisitAndRebuild(array, fun))

  override def xmlPrinter: Elem =
    <zipAcc2 n={ToString(n)} dt1={ToString(dt1)} dt2={ToString(dt2)}>
      {Phrases.xmlPrinter(array)}
    </zipAcc2>

  override def prettyPrint: String = s"(ZipAcc2 ${PrettyPhrasePrinter(array)})"
}
