package Core

trait Pattern {
  def typeCheck(): ExpType

  def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): Pattern

  def eval(s: OperationalSemantics.Store): OperationalSemantics.Data
}
