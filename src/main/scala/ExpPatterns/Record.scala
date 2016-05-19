package ExpPatterns

import Core._
import Core.OperationalSemantics._

case class Record(fst: Phrase[ExpType], snd: Phrase[ExpType]) extends ExpPattern {

  override def typeCheck(): ExpType = {
    ExpType(RecordType( TypeChecker(fst).dataType, TypeChecker(snd).dataType ))
  }

  override def eval(s: Store): Data = {
    RecordData(
      OperationalSemantics.eval(s, fst),
      OperationalSemantics.eval(s, snd))
  }

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): ExpPattern = {
    Record(
      OperationalSemantics.substitute(phrase, `for`, fst),
      OperationalSemantics.substitute(phrase, `for`, snd))
  }

  override def toC = {
    val dt = typeCheck() match { case ExpType(dataType) => dataType }
    s"(struct ${Printer.nameOf(dt)}){ ${Printer.toC(fst)} , ${Printer.toC(snd)} }"
  }

  override def prettyPrint: String = s"(${PrettyPrinter(fst)}, ${PrettyPrinter(snd)})"

}
