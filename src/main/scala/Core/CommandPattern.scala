package Core

trait CommandPattern {
  def typeCheck(): CommandType

  def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): CommandPattern

  def eval(s: OperationalSemantics.Store): OperationalSemantics.Store

  def toC: String

  def asPhrase = CommandPatternPhrase(this)
}
