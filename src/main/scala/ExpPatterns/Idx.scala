package ExpPatterns

import Core._
import Core.OperationalSemantics._

case class Idx(array: Phrase[ExpType], index: Phrase[ExpType]) extends ExpPattern {

  override def typeCheck(): ExpType = {
    import TypeChecker._
    check(TypeChecker(index), ExpType(int))
    TypeChecker(array) match {
      case ExpType(ArrayType(n, t)) => ExpType(t)
      case t => error(t.toString, "ArrayType")
    }
  }

  override def eval(s: Store): Data = {
    (OperationalSemantics.eval(s, array), OperationalSemantics.eval(s, index)) match {
      case (ArrayData(xs), IntData(i)) => xs(i)
      case _ => throw new Exception("This should not happen")
    }
  }

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): ExpPattern = {
    Idx(
      OperationalSemantics.substitute(phrase, `for`, array),
      OperationalSemantics.substitute(phrase, `for`, index))
  }

  override def toC = Printer.toC(array) + "[" + Printer.toC(index) + "]"

}
