package AccPatterns

import Core._
import Core.OperationalSemantics._
import apart.arithmetic.ArithExpr

case class JoinAcc(n: ArithExpr, array: Phrase[AccType]) extends AccPattern {

  override def typeCheck(): AccType = {
    import TypeChecker._
    TypeChecker(array) match {
      case AccType(ArrayType(m, dt)) =>
        AccType(ArrayType(m/n, ArrayType(n, dt)))
      case t => error(t.toString, "ArrayType")
    }
  }

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): AccPattern = {
    JoinAcc(n, OperationalSemantics.substitute(phrase, `for`, array))
  }

  override def eval(s: Store): AccIdentifier = ???

  override def toC = ???

  override def prettyPrint: String = s"(join ${n.toString} ${PrettyPrinter(array)})"

}
