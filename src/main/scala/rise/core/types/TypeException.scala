package rise.core.types

case class TypeException(msg: String) extends Exception {
  override def toString: String = s"type exception: $msg"
}
