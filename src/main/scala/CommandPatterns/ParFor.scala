package CommandPatterns

import Core._
import Core.OperationalSemantics._
import Core.PhraseType._
import Rewriting.SubstituteImplementations

case class ParFor(n: Phrase[ExpType],
                  out: Phrase[AccType],
                  body: Phrase[ExpType -> (AccType -> CommandType)]) extends CommandPattern {

  override def typeCheck(): CommandType = {
    import TypeChecker._

    check(TypeChecker(n), ExpType(int))
    val nInt = OperationalSemantics.evalIntExp(new OperationalSemantics.Store(), n)

    TypeChecker(out) match {
      case AccType(ArrayType(m, dt)) =>
        if (nInt == m) {
          TypeChecker(body) match {
            case FunctionType(ExpType(i), FunctionType(AccType(dt2), CommandType())) =>
              if (i == int && dt == dt2) {
                CommandType()
              } else error(s"$i, $dt, and $dt2", expected = "to be int and the last two to match")
            case t_ => error(t_.toString, expected = "FunctionType")
          }
        } else error("$nInt != $m", expected = "them to match")
      case t_ => error(t_.toString, expected = "ArrayType")
    }
  }

  override def eval(s: Store): Store = ???

  override def toC: String = {
    val bodyE = Lift.liftFunction(body)
    val i = IdentPhrase[ExpType](OperationalSemantics.newName())
    i.t = ExpType(int)
    s"for (int ${i.name} = 0; ${i.name} < ${Printer.toC(n)}; ++${i.name}) {\n${Printer.toC(bodyE(i))}}\n"
  }

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): CommandPattern = {
    ParFor(
      OperationalSemantics.substitute(phrase, `for`, n),
      OperationalSemantics.substitute(phrase, `for`, out),
      OperationalSemantics.substitute(phrase, `for`, body))
  }

  override def substituteImpl: Phrase[CommandType] = ParFor(n, out, SubstituteImplementations.applyBinaryFun(body))

  override def prettyPrint: String = s"parFor ${PrettyPrinter(n)} ${PrettyPrinter(out)} ${PrettyPrinter(body)}"

}
