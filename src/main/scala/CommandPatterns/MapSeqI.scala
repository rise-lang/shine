package CommandPatterns

import Core.PhraseType._
import Core._

import DSL._

case class MapSeqI(out: Phrase[AccType],
                   f: Phrase[AccType -> (ExpType -> CommandType)],
                   in: Phrase[ExpType]) extends AbstractMapI(out, f, in) {

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): CommandPattern = {
    MapSeqI(
      OperationalSemantics.substitute(phrase, `for`, out),
      OperationalSemantics.substitute(phrase, `for`, f),
      OperationalSemantics.substitute(phrase, `for`, in))
  }

  override def toC = ???

  override def prettyPrint: String = s"mapSeqI ${PrettyPrinter(out)} ${PrettyPrinter(f)} ${PrettyPrinter(in)}"

  override def substituteImpl: Phrase[CommandType] = {
    `for`(length(in), i => {
      f(out `@` i)(in `@` i)
    })
  }

}
