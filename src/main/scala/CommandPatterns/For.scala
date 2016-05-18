package CommandPatterns

import Core._
import Core.OperationalSemantics._
import Core.PhraseType._

case class For(n: Phrase[ExpType], body: Phrase[ExpType -> CommandType]) extends CommandPattern {

  override def typeCheck(): CommandType = {
    import TypeChecker._
    check(TypeChecker(n), ExpType(int))
    check(TypeChecker(body), FunctionType(ExpType(int), CommandType()))
    CommandType()
  }

  override def eval(s: Store): Store = {
    val nE = evalIntExp(s, n)
    val bodyE = OperationalSemantics.eval(s, body)
    (0 until nE).foldLeft(s)( (s1, i) => {
      OperationalSemantics.eval(s1, bodyE(LiteralPhrase(i)))
    } )
  }

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): CommandPattern = {
    For(
      OperationalSemantics.substitute(phrase, `for`, n),
      OperationalSemantics.substitute(phrase, `for`, body))
  }

  override def toC = {
    val bodyE = Lift.liftFunction(body)
    val i = IdentPhrase[ExpType](OperationalSemantics.newName())
    i.t = ExpType(int)
    s"for (int ${i.name} = 0; ${i.name} < ${Printer.toC(n)}; ++${i.name}) {\n${Printer.toC(bodyE(i))}}\n"
  }

}
