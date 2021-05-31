package rise.core

object ForeignFunction {
  case class Decl(name: String, definition: Option[Def])
  case class Def(params: Seq[String], body: String)
}
