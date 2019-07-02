package idealised.DPIA.Types

sealed trait AddressSpace

object AddressSpace {
  object Global extends AddressSpace { override def toString = "Global" }

  object Local extends AddressSpace { override def toString = "Local" }

  object Private extends AddressSpace { override def toString = "Private" }

  object Constant extends AddressSpace { override def toString = "Constant" }

  //Maybe not needed
  //final case class RecordAddrSpace(fst: AddrSpace, snd: AddrSpace) extends AddrSpace {
  //  override def toString: String = s"($fst x $snd)"
  //}
}

final case class AddressSpaceIdentifier(name: String) extends AddressSpace with Kind.Identifier {
  override def toString: String = name
}
