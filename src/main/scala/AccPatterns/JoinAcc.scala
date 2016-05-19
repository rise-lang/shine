package AccPatterns

import Core._
import Core.OperationalSemantics._

case class JoinAcc(array: Phrase[AccType]) extends AccPattern {

  override def typeCheck(): AccType = {
    import TypeChecker._
    TypeChecker(array) match {
      case AccType(ArrayType(n, ArrayType(m, dt))) =>
        AccType(ArrayType(n*m, dt))
      case t => error(t.toString, "ArrayType(ArrayType)")
    }
  }

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): AccPattern = {
    JoinAcc(OperationalSemantics.substitute(phrase, `for`, array))
  }

  override def eval(s: Store): AccIdentifier = {
    ???
  }

  override def toC = ???

  override def prettyPrint: String = s"(join ${PrettyPrinter(array)})"

}
