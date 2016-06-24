package Core

sealed trait AddressSpace

object GlobalMemory extends AddressSpace { override def toString = "globalMemory" }
object LocalMemory extends AddressSpace { override def toString = "localMemory" }
object PrivateMemory extends AddressSpace { override def toString = "privateMemory" }

object AddressSpace {
  def toOpenCL(addressSpace: AddressSpace): opencl.ir.OpenCLAddressSpace = {
    addressSpace match {
      case GlobalMemory => opencl.ir.GlobalMemory
      case LocalMemory => opencl.ir.LocalMemory
      case PrivateMemory => opencl.ir.PrivateMemory
    }
  }
}
