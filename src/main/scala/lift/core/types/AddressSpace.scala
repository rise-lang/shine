package lift.core.types

// TODO: should not be in the core
sealed trait AddressSpace

object AddressSpace {
  object Global extends AddressSpace { override def toString = "Global" }

  object Local extends AddressSpace { override def toString = "Local" }

  object Private extends AddressSpace { override def toString = "Private" }

  object Constant extends AddressSpace { override def toString = "Constant" }
}

final case class AddressSpaceIdentifier(name: String) extends AddressSpace with Kind.Identifier {
  override def toString: String = name
}
