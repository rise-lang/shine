package shine.DPIA.primitives.imperative

import shine.DPIA.Phrases.CommandPrimitive
import shine.macros.Primitive.comPrimitive

@comPrimitive
final case class Comment(comment : String) extends CommandPrimitive {
  override def prettyPrint: String = s"\n//$comment\n"
}
