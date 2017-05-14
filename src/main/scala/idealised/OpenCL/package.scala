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

  trait FunctionHelper {
    type T
    type R
    type F = T => R
  }

  type =:=>[TT, RR] = FunctionHelper { type T = TT; type R = RR }

  sealed trait HList {
    def length: Int
    def toList: List[Any]
  }

  case class HCons[+H, +T <: HList](head: H, tail: T) extends HList {
    def :: [V](v: V) = HCons(v, this)

    override def length: Int = tail.length + 1
    override def toList: List[Any] = List(head) ++ tail.toList
  }

  case object HNil extends HList {
    def ::[V](v: V) = HCons(v, this)

    override def length: Int = 0
    override def toList: List[Nothing] = List()
  }

  implicit class HNilHelper[V](v1: V) {
    def ::[V2](v2: V2) = HCons(v1, HCons(v2, HNil))
  }

  type ::[H, T <: HList] = HCons[H, T]

  type Nil = HNil.type

}
