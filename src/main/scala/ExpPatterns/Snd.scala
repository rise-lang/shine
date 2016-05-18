package ExpPatterns

import Core._
import Core.OperationalSemantics._

case class Snd(record: Phrase[ExpType]) extends ExpPattern {

  override def typeCheck(): ExpType = {
    TypeChecker(record) match {
      case ExpType(RecordType(fst, snd)) => ExpType(snd)
      case t => TypeChecker.error(t.toString, "Something else")
    }
  }

  override def eval(s: Store): Data = {
    OperationalSemantics.eval(s, record) match {
      case r: RecordData => r.snd
      case _ => throw new Exception("This should not happen")
    }
  }

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): ExpPattern = {
    Snd(OperationalSemantics.substitute(phrase, `for`, record))
  }

  override def toC = Printer.toC(record) + ".snd"

}
