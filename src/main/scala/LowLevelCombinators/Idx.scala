package LowLevelCombinators

import Core.OperationalSemantics._
import Core._

import scala.xml.Elem

final case class Idx(n: Nat,
                     dt: DataType,
                     index: Phrase[ExpType],
                     array: Phrase[ExpType])
  extends LowLevelExpCombinator {

  override lazy val `type` = exp"[$dt]"

  override def typeCheck(): Unit = {
    import TypeChecker._
    (n: Nat) -> (dt: DataType) ->
      (index `:` exp"[$int]") ->
      (array `:` exp"[$n.$dt]") ->
      `type`
  }

  override def inferTypes: Idx = {
    import TypeInference._
    val index_ = TypeInference(index)
    val array_ = TypeInference(array)
    array_.t match {
      case ExpType(ArrayType(n_, dt_)) => Idx(n_, dt_, index_, array_)
      case x => error(x.toString, "ExpType(ArrayType)")
    }
  }

  override def eval(s: Store): Data = {
    (OperationalSemantics.eval(s, array), OperationalSemantics.eval(s, index)) match {
      case (ArrayData(xs), IntData(i)) => xs(i)
      case _ => throw new Exception("This should not happen")
    }
  }

  override def visitAndRebuild(fun: VisitAndRebuild.fun): Phrase[ExpType] = {
    Idx(fun(n), fun(dt), VisitAndRebuild(index, fun), VisitAndRebuild(array, fun))
  }

  override def prettyPrint: String = s"(${PrettyPrinter(array)})[${PrettyPrinter(index)}]"

  override def xmlPrinter: Elem =
    <idx n={ToString(n)} dt={ToString(dt)}>
      <input type={ToString(ExpType(ArrayType(n, dt)))}>
        {Core.xmlPrinter(array)}
      </input>
      <index type={ToString(ExpType(int))}>
        {Core.xmlPrinter(index)}
      </index>
    </idx>

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = ???

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = ???
}
