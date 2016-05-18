package AccPatterns

import Core._
import Core.OperationalSemantics._

case class SplitAccPattern(n: Int, array: Phrase[AccType]) extends AccPattern {

  override def typeCheck(): AccType = {
    import TypeChecker._
    TypeChecker(array) match {
      case AccType(ArrayType(m, dt)) =>
        AccType(ArrayType(m/n, ArrayType(n, dt)))
      case t => error(t.toString, "ArrayType")
    }
  }

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): AccPattern = {
    SplitAccPattern(n, OperationalSemantics.substitute(phrase, `for`, array))
  }

  override def eval(s: Store): AccIdentifier = {
    ???
  }

}
