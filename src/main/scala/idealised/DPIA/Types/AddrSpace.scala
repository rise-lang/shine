package idealised.DPIA.Types

sealed trait AddrSpace

object PrivateMem extends AddrSpace { override def toString = "Private" }

object LocalMem extends AddrSpace { override def toString = "Local" }

object GlobalMem extends AddrSpace { override def toString = "Global" }

object UnknownMem extends AddrSpace { override def toString = "_" }

//Maybe not needed
//final case class RecordAddrSpace(fst: AddrSpace, snd: AddrSpace) extends AddrSpace {
//  override def toString: String = s"($fst x $snd)"
//}

final case class AddrSpaceIdentifier(name: String) extends AddrSpace with Kind.Identifier {
  override def toString: String = name
}
