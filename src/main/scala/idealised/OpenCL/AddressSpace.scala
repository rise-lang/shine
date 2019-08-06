package idealised.OpenCL

sealed trait AddressSpace
case object GlobalMemory extends AddressSpace
case object LocalMemory extends AddressSpace
case object PrivateMemory extends AddressSpace
