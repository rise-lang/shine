package idealised.ImperativePrimitives

import idealised._
import idealised.Core._
import idealised.Core.OperationalSemantics._

import scala.xml.Elem

final case class ZipAcc1(n: Nat,
                         dt1: DataType,
                         dt2: DataType,
                         array: Phrase[AccType])
  extends AccPrimitive {

  override lazy val `type`: AccType = acc"[$n.$dt1]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    (n: Nat) -> (dt1: DataType) -> (dt2: DataType) ->
      (array :: acc"[$n.($dt1 x $dt2)]") -> `type`
  }

  override def eval(s: Store): AccIdentifier = ???

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] =
    ZipAcc1(fun(n), fun(dt1), fun(dt2), VisitAndRebuild(array, fun))

  override def xmlPrinter: Elem =
    <zipAcc1 n={ToString(n)} dt1={ToString(dt1)} dt2={ToString(dt2)}>
      {Core.xmlPrinter(array)}
    </zipAcc1>

  override def prettyPrint: String = s"(ZipAcc1 ${PrettyPhrasePrinter(array)})"
}
