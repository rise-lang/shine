package Core

trait ExpPattern {
  def typeCheck(): ExpType

  def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): ExpPattern

  def eval(s: OperationalSemantics.Store): OperationalSemantics.Data

  def toC: String

  def prettyPrint: String

  def asPhrase = ExpPatternPhrase(this)
}
