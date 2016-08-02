package idealised

package object OpenCL {
  sealed trait ParallelismLevel
  case object WorkGroup extends ParallelismLevel
  case object Global extends ParallelismLevel
  case object Local extends ParallelismLevel
  case object Sequential extends ParallelismLevel


  sealed trait AddressSpace extends idealised.Core.AddressSpace
  case object GlobalMemory extends AddressSpace
  case object LocalMemory extends AddressSpace
  case object PrivateMemory extends AddressSpace

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
