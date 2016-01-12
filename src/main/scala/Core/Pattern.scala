package Core

trait Pattern {
  def typeCheck(): ExpType

  def substitute[T <: PhraseType](p1: Phrase[T], p2: Phrase[T]): Pattern

  def eval(s: OperationalSemantics.Store): OperationalSemantics.Data
}
