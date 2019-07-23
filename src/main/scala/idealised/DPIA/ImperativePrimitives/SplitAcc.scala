package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.Compilation.CodeGenerator
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

final case class SplitAcc(n: Nat,
                          m: Nat,
                          dt: DataType,
                          array: Phrase[AccType])
  extends AccPrimitive {

  override val t: AccType =
    (n: Nat) ->: (m: Nat) ->: (dt: DataType) ->:
      (array :: acc"[$m.$n.$dt]") ->:
        acc"[${n * m}.$dt]"

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] = {
    SplitAcc(fun.nat(n), fun.nat(m), fun.data(dt), VisitAndRebuild(array, fun))
  }

  override def eval(s: Store): AccIdentifier = ???

  override def prettyPrint: String = s"(splitAcc ${PrettyPhrasePrinter(array)})"

  override def xmlPrinter: Elem =
    <splitAcc n={ToString(n)} m={ToString(m)} dt={ToString(dt)}>
      {Phrases.xmlPrinter(array)}
    </splitAcc>
}
