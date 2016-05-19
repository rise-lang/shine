package CommandPatterns

import Core._
import Core.OperationalSemantics._

case class Skip() extends CommandPattern {

  override def typeCheck(): CommandType = CommandType()

  override def eval(s: Store): Store = s

  override def substitute[T <: PhraseType](phrase: Phrase[T], `for`: Phrase[T]): CommandPattern = this

  override def substituteImpl: Phrase[CommandType] = this.asPhrase

  override def toC = ""

  override def prettyPrint: String = ""

}
