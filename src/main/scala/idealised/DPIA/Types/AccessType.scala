package idealised.DPIA.Types

sealed trait AccessType {
  override def equals(o: Any): Boolean = o match {
    case _: AccessType => true // TODO: remove when accesses work
    case _ => false
  }
}

object write extends AccessType { override def toString = "write" }

object read extends AccessType { override def toString = "read" }

final case class AccessTypeIdentifier(name: String) extends AccessType with Kind.Identifier {
  override def toString: String = name
}
