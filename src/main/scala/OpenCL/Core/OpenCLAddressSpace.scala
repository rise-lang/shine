package OpenCL.Core

import Core.AddressSpace

sealed trait OpenCLAddressSpace extends AddressSpace

object GlobalMemory extends OpenCLAddressSpace { override def toString = "globalMemory" }
object LocalMemory extends OpenCLAddressSpace { override def toString = "localMemory" }
object PrivateMemory extends OpenCLAddressSpace { override def toString = "privateMemory" }

object OpenCLAddressSpace {
  def toOpenCL(addressSpace: OpenCLAddressSpace): opencl.ir.OpenCLAddressSpace = {
    addressSpace match {
      case GlobalMemory => opencl.ir.GlobalMemory
      case LocalMemory => opencl.ir.LocalMemory
      case PrivateMemory => opencl.ir.PrivateMemory
    }
  }
}
