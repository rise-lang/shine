package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

final case class TransposeAcc(n: Nat, m: Nat, dt: DataType,
                              array: Phrase[AccType])
  extends AccPrimitive
{
  override val t: AccType =
    (n: Nat) ->: (m: Nat) ->: (dt: DataType) ->:
      (array :: acc"[$m.$n.$dt]") ->:
        acc"[$n.$m.$dt]"

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] = {
    TransposeAcc(fun.nat(n), fun.nat(m), fun.data(dt), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): AccIdentifier = ???

  override def prettyPrint: String =
    s"(transposeAcc ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <transposeAcc n={ToString(n)} m={ToString(m)} dt={ToString(dt)}>
      {Phrases.xmlPrinter(array)}
    </transposeAcc>
}
