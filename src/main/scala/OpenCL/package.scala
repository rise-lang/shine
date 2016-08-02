
package object OpenCL {
  sealed trait ParallelismLevel
  case object WorkGroup extends ParallelismLevel
  case object Global extends ParallelismLevel
  case object Local extends ParallelismLevel
  case object Sequential extends ParallelismLevel


  sealed trait AddressSpace extends _root_.Core.AddressSpace
  case object GlobalMemory extends AddressSpace { override def toString = "globalMemory" }
  case object LocalMemory extends AddressSpace { override def toString = "localMemory" }
  case object PrivateMemory extends AddressSpace { override def toString = "privateMemory" }

  object AddressSpace {
    def toOpenCL(addressSpace: AddressSpace): opencl.ir.OpenCLAddressSpace = {
      addressSpace match {
        case GlobalMemory => opencl.ir.GlobalMemory
        case LocalMemory => opencl.ir.LocalMemory
        case PrivateMemory => opencl.ir.PrivateMemory
      }
    }
  }
}
