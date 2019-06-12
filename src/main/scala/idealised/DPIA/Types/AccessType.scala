package idealised.DPIA.Types

sealed trait AccessType

object Write extends AccessType { override def toString = "Write" }

object Read extends AccessType { override def toString = "Read" }

final case class AccessTypeIdentifier(name: String) extends AccessType with Kind.Identifier {
  override def toString: String = name
}
