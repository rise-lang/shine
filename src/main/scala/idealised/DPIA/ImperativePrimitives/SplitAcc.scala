package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types.{AccType, DataType}
import idealised.DPIA._

import scala.xml.Elem

final case class SplitAcc(n: Nat,
                          m: Nat,
                          dt: DataType,
                          array: Phrase[AccType])
  extends AccPrimitive {

  override lazy val `type` = acc"[${n * m}.$dt]"

  override def typeCheck(): Unit = {
    import idealised.DPIA.Types.TypeChecker._
    (n: Nat) -> (m: Nat) -> (dt: DataType) ->
      (array :: acc"[$n.$m.$dt]") -> `type`
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] = {
    SplitAcc(fun(n), fun(m), fun(dt), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): AccIdentifier = ???

  override def prettyPrint: String = s"(splitAcc ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <splitAcc n={ToString(n)} m={ToString(m)} dt={ToString(dt)}>
      {Phrases.xmlPrinter(array)}
    </splitAcc>
}
