package shine.DPIA.Types

sealed trait AccessType

object write extends AccessType { override def toString = "write" }

object read extends AccessType { override def toString = "read" }

final case class AccessTypeIdentifier(name: String) extends AccessType {
  override def toString: String = name
}
