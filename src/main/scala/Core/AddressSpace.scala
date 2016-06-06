package Core

sealed trait AddressSpace

object GlobalMemory extends AddressSpace { override def toString = "globalMemory" }
object LocalMemory extends AddressSpace { override def toString = "localMemory" }
object PrivateMemory extends AddressSpace { override def toString = "privateMemory" }
