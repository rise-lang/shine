package Core

trait AccPattern {
  def typeCheck(): AccType

  def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): AccPattern

  def eval(s: OperationalSemantics.Store): OperationalSemantics.AccIdentifier

  def toC: String

  def asPhrase = AccPatternPhrase(this)
}
