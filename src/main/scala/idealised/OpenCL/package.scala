package idealised

import lift.arithmetic.ArithExpr

package object OpenCL {
  sealed trait ParallelismLevel
  case object WorkGroup extends ParallelismLevel
  case object Global extends ParallelismLevel
  case object Local extends ParallelismLevel
  case object Sequential extends ParallelismLevel


  sealed trait AddressSpace extends DPIA.AddressSpace
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

  case class NDRange(x: ArithExpr, y: ArithExpr, z: ArithExpr)

  trait FunctionHelper {
    type T
    type R
    type F = T => R
  }


  type `)=>`[TT, RR] = FunctionHelper { type T = TT; type R = RR }

  sealed trait HList {
    def length: Int
    def toList: List[Any]
  }

  case class HCons[+L <: HList, +N](list: L, node: N) extends HList {
    def `,`[V](v: V) = HCons(this, v)

    override def length: Int = list.length + 1
    override def toList: List[Any] = list.toList :+ node
  }

  case object HNil extends HList {
    def `,`[V](v: V) = HCons(this, v)

    override def length: Int = 0
    override def toList: List[Nothing] = List()
  }

  type `,`[L <: HList, N] = HCons[L, N]

  type `(`[L <: ScalaFunction, N] = HCons[L, N]

  implicit class HNilHelper[V](v1: V) {
    def `,`[V2](v2: V2) = HCons(HCons(HNil, v1), v2)

    def `;` = HCons(HNil, v1)
  }

  type ScalaFunction = HNil.type
}
