package ExpPatterns

import Core._
import Core.OperationalSemantics._
import Core.PhraseType.->

case class Length[T <: BasePhraseTypes](array: Phrase[T]) extends ExpPattern {

  override def typeCheck(): ExpType = {
    import TypeChecker._
    TypeChecker(array) match {
      case ExpType(ArrayType(n, t)) => ExpType(int)
      case AccType(ArrayType(n, t)) => ExpType(int)
      case t => error(t.toString, "ArrayType")
    }
  }

  override def eval(s: Store): Data = {
    array.t match {
      case ExpType(ArrayType(n, _)) => IndexData(n)
      case AccType(ArrayType(n, _)) => IndexData(n)
    }
  }

  override def substitute[U <: PhraseType](phrase: Phrase[U], `for`: Phrase[U]): ExpPattern = {
    Length(OperationalSemantics.substitute(phrase, `for`, array))
  }

  override def toC = array.t match {
    case ExpType(ArrayType(n, dt)) => n.toString
    case AccType(ArrayType(n, dt)) => n.toString
  }

  override def prettyPrint: String = s"(length ${PrettyPrinter(array)})"

  override def rewriteToImperativeAcc(A: Phrase[AccType]): Phrase[CommandType] = ???

  override def rewriteToImperativeExp(C: Phrase[->[ExpType, CommandType]]): Phrase[CommandType] = ???
}
