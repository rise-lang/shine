package shine.DPIA.ImperativePrimitives

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

import scala.xml.Elem

final case class AsScalarAcc(n: Nat,
                             m: Nat,
                             dt: ScalarType,
                             array: Phrase[AccType])
  extends AccPrimitive {

  override val t: AccType =
    (n: Nat) ~>: (m: Nat) ~>: (dt: ScalarType) ~>:
      (array :: accT((m * n)`.`dt)) ~>:
        accT(n`.`vec(m, dt))

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] = {
    AsScalarAcc(fun.nat(n), fun.nat(m), fun.data(dt), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): AccIdentifier = ???

  override def prettyPrint = s"(asScalarAcc $n ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <asScalarAcc n={ToString(n)} m={ToString(m)}>
      {Phrases.xmlPrinter(array)}
    </asScalarAcc>
}
