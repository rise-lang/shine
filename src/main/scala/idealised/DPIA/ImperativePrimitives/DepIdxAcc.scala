package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.Compilation.CodeGenerator
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

final case class DepIdxAcc(n: Nat,
                           ft:NatToData,
                           index: Nat,
                           array: Phrase[AccType])
  extends AccPrimitive {

  override val t: AccType =
    (n: Nat) -> (ft:NatToData) -> (index: Nat) ->
      (array :: acc"[$n.$ft]") ->
        acc"[${ft(index)}]"

  override def eval(s: Store): AccIdentifier = {
//    val arrayE = OperationalSemantics.eval(s, array)
//    val indexE = OperationalSemantics.eval(s, index) match {
//      case IntData(i) => i
//      case _ => throw new Exception("This should not happen")
//    }
//    ArrayAccessIdentifier(arrayE, indexE)
    ???
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] = {
    DepIdxAcc(fun(n), fun(ft), fun(index), VisitAndRebuild(array, fun))
  }

  override def prettyPrint: String = s"${PrettyPhrasePrinter(array)}[$index]"

  override def xmlPrinter: Elem =
    <depIdxAcc n={ToString(n)} ft={ToString(ft)} index={ToString(index)}>
      <input type={ToString(array.t)}>
        {Phrases.xmlPrinter(array)}
      </input>
    </depIdxAcc>
}

