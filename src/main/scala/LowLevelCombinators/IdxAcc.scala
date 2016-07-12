package LowLevelCombinators

import Core.OperationalSemantics._
import Core._

import scala.xml.Elem

case class IdxAcc(n: Nat,
                  dt: DataType,
                  index: Phrase[ExpType],
                  array: Phrase[AccType])
  extends LowLevelAccCombinator with TypeInferable {

  override lazy val `type` = acc"[$dt]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    (n: Nat) -> (dt: DataType) ->
      (index `:` exp"[$int]") ->
      (array `:` acc"[$n.$dt]") -> `type`
  }

  override def inferTypes: IdxAcc = {
    import TypeInference._
    val index_ = TypeInference(index)
    val array_ = TypeInference(array)
    array_.t match {
      case AccType(ArrayType(n_, dt_)) => IdxAcc(n_, dt_, index_, array_)
      case x => error(x.toString, "ExpType(ArrayType)")
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

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[AccType] = {
    IdxAcc(fun(n), fun(dt), VisitAndRebuild(index, fun), VisitAndRebuild(array, fun))
  }

  override def prettyPrint: String = s"${PrettyPrinter(array)}[${PrettyPrinter(index)}]"

  override def xmlPrinter: Elem =
    <idxAcc n={ToString(n)} dt={ToString(dt)}>
      <output>{Core.xmlPrinter(array)}</output>
      <index>{Core.xmlPrinter(index)}</index>
    </idxAcc>
}
