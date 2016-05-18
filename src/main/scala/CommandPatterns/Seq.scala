package CommandPatterns

import Core._
import Core.OperationalSemantics._

case class Seq(c1: Phrase[CommandType], c2: Phrase[CommandType]) extends CommandPattern {

  override def typeCheck(): CommandType = {
    import TypeChecker._
    check(TypeChecker(c1), CommandType())
    check(TypeChecker(c2), CommandType())
    CommandType()
  }

  override def eval(s: Store): Store = {
    val s1 = OperationalSemantics.eval(s, c1)
    OperationalSemantics.eval(s1, c2)
  }

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): CommandPattern = {
    Seq(
      OperationalSemantics.substitute(phrase, `for`, c1),
      OperationalSemantics.substitute(phrase, `for`, c2))
  }

  override def toC = Printer.toC(c1) + ";\n" + Printer.toC(c2)

}
