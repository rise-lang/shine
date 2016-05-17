package Core

trait AccPattern {
  def typeCheck(): ExpType

  def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): AccPattern

  def eval(s: OperationalSemantics.Store): OperationalSemantics.AccIdentifier

  def asPhrase = AccPatternPhrase(this)
}
