package lift.core.types

sealed trait AccessType

object W extends AccessType { override def toString = "W" }

object R extends AccessType { override def toString = "R" }

final case class AccessTypeIdentifier(name: String) extends AccessType with Kind.Identifier {
  override def toString: String = name
}
