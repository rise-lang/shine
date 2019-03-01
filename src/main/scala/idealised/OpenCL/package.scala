package idealised

import idealised.DPIA.Nat
import lift.arithmetic._

package object OpenCL {
  sealed trait ParallelismLevel
  case object WorkGroup extends ParallelismLevel
  case object Global extends ParallelismLevel
  case object Local extends ParallelismLevel
  case object Sequential extends ParallelismLevel


  sealed trait AddressSpace
  case object GlobalMemory extends AddressSpace
  case object LocalMemory extends AddressSpace
  case object PrivateMemory extends AddressSpace

  case class NDRange(x: Nat, y: Nat, z: Nat)

  // This class models OpenCL built in functions that can appear inside of arithmetic expressions
  // examples are get_global_size(0), or get_local_id(1), but also OpenCL math functions, e.g., ceil or sin
  class BuiltInFunction private(name: String, val param: Int, range: Range)
    extends ArithExprFunction(name, range) {

    lazy val toOCLString = s"$name($param)"

    override lazy val digest: Int = HashSeed ^ /*range.digest() ^*/ name.hashCode ^ param

    override val HashSeed = 0x31111111

    override def equals(that: Any): Boolean = that match {
      case f: BuiltInFunction => this.name.equals(f.name) && this.param == f.param
      case _ => false
    }

    override lazy val (min : Nat, max: Nat) = (range.min.min, range.max.max)
    override lazy val sign: Sign.Value = Sign.Positive

    override def visitAndRebuild(f: Nat => Nat): Nat =
      f(new BuiltInFunction(name, param, range.visitAndRebuild(f)))

  }

  object BuiltInFunction {
    def apply(name: String, param: Int, range: Range = RangeUnknown) : BuiltInFunction =
      new BuiltInFunction(name, param, range)
  }

  object get_num_groups {
    def apply(param:Int, range : Range = ContinuousRange(1, PosInf)) =
      BuiltInFunction("get_num_groups", param, range)
  }

  object get_global_size {
    def apply(param: Int, range : Range = ContinuousRange(1, PosInf)) =
      BuiltInFunction("get_global_size", param, range)
  }

  object get_local_size {
    def apply(param: Int, range : Range = ContinuousRange(1, PosInf)) =
      BuiltInFunction("get_local_size", param, range)
  }

  object get_local_id {
    def apply(param:Int, range : Range) =
      BuiltInFunction("get_local_id", param, range)

    def apply(param:Int) =
      BuiltInFunction("get_local_id", param, ContinuousRange(0, get_local_size(param)))
  }

  object get_global_id {
    def apply(param:Int, range : Range) =
      BuiltInFunction("get_global_id", param, range)

    def apply(param:Int) =
      BuiltInFunction("get_global_id", param, ContinuousRange(0, get_global_size(param)))
  }

  object get_group_id {
    def apply(param:Int, range : Range) =
      BuiltInFunction("get_group_id", param, range)

    def apply(param:Int) =
      BuiltInFunction("get_group_id", param, ContinuousRange(0, get_num_groups(param)))

  }

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
