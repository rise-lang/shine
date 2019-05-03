package idealised.DPIA.ImperativePrimitives

import idealised.DPIA.Compilation.CodeGenerator
import idealised.DPIA.Phrases._
import idealised.DPIA.Semantics.OperationalSemantics
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA.Types._
import idealised.DPIA._

import scala.xml.Elem

final case class IdxAcc(n: Nat,
                        dt: DataType,
                        index: Phrase[ExpType],
                        array: Phrase[AccType])
  extends AccPrimitive {

  override val t: AccType =
    (n: Nat) -> (dt: DataType) ->
      (index :: exp"[idx($n)]") ->
        (array :: acc"[$n.$dt]") ->
          acc"[$dt]"

  override def eval(s: Store): AccIdentifier = {
    val arrayE = OperationalSemantics.eval(s, array)
    val indexE = OperationalSemantics.eval(s, index) match {
      case IntData(i) => i
      case _ => throw new Exception("This should not happen")
    }
    ArrayAccessIdentifier(arrayE, indexE)
  }

  override def visitAndRebuild(fun: VisitAndRebuild.Visitor): Phrase[AccType] = {
    IdxAcc(fun(n), fun(dt), VisitAndRebuild(index, fun), VisitAndRebuild(array, fun))
  }

  override def prettyPrint: String = s"${PrettyPhrasePrinter(array)}[${PrettyPhrasePrinter(index)}]"

  override def xmlPrinter: Elem =
    <idxAcc n={ToString(n)} dt={ToString(dt)}>
      <output>
        {Phrases.xmlPrinter(array)}
      </output>
      <index>
        {Phrases.xmlPrinter(index)}
      </index>
    </idxAcc>
}

object IdxAcc {
  def apply(index: Phrase[ExpType],
            array: Phrase[AccType]): IdxAcc = {
    (index.t, array.t) match {
      case (ExpType(IndexType(n1)), AccType(ArrayType(n2, dt_))) if n1 == n2 =>
        IdxAcc(n1, dt_, index, array)
      case x => error(x.toString, "(exp[idx(n)], exp[n.dt])")
    }
  }
}