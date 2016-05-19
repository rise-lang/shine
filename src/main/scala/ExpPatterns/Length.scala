package ExpPatterns

import Core._
import Core.OperationalSemantics._

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
      case ExpType(ArrayType(n, _)) => n
      case AccType(ArrayType(n, _)) => n
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

}
