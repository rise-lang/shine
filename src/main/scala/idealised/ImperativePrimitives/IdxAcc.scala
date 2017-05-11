package idealised.ImperativePrimitives

import idealised._
import idealised.Core._
import idealised.Core.OperationalSemantics._

import scala.xml.Elem

final case class IdxAcc(n: Nat,
                        dt: DataType,
                        index: Phrase[ExpType],
                        array: Phrase[AccType])
  extends AccPrimitive with TypeInferable[AccType] {

  override lazy val `type` = acc"[$dt]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    (n: Nat) -> (dt: DataType) ->
      (index :: exp"[idx($n)]") ->
      (array :: acc"[$n.$dt]") -> `type`
  }

  override def inferTypes: IdxAcc = {
    import TypeInference._
    val index_ = TypeInference(index)
    val array_ = TypeInference(array)
    (index_.t, array_.t) match {
      case (ExpType(IndexType(n1)), AccType(ArrayType(n2, dt_))) if n1 == n2 =>
        IdxAcc(n1, dt_, index_, array_)
      case x => error(x.toString, "(exp[idx(n)], exp[n.dt])")
    }
  }

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
        {Core.xmlPrinter(array)}
      </output>
      <index>
        {Core.xmlPrinter(index)}
      </index>
    </idxAcc>
}
