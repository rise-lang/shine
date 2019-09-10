package idealised

import com.github.ghik.silencer.silent
import idealised.DPIA.Nat
import lift.arithmetic._

import scala.language.implicitConversions

package object OpenCL {

  implicit def valToNatTuple[V](v: V)(implicit vToN: V => Nat): NDRange = NDRange(v, 1, 1)
  implicit def pairToNatTuple[A,B](t: (A, B))(implicit aToN: A => Nat, bToN: B => Nat): NDRange =
    NDRange(t._1, t._2, 1)
  implicit def tripleToNatTuple[T,U,V](t: (T, U, V))
                                      (implicit aToN: T => Nat, bToN: U => Nat, cToN: V => Nat): NDRange =
    NDRange(t._1, t._2, t._3)
  implicit def tupleToNDRange[R](ndRange: R)
                                (implicit intToNat: R => (Nat, Nat, Nat)): NDRange =
    NDRange(ndRange._1, ndRange._2, ndRange._3)

  object get_num_groups {
    def apply(param:Int, range : Range = ContinuousRange(1, PosInf)) =
      BuiltInFunctionCall("get_num_groups", param, range)
  }

  object get_global_size {
    def apply(param: Int, range : Range = ContinuousRange(1, PosInf)) =
      BuiltInFunctionCall("get_global_size", param, range)
  }

  object get_local_size {
    def apply(param: Int, range : Range = ContinuousRange(1, PosInf)) =
      BuiltInFunctionCall("get_local_size", param, range)
  }

  object get_local_id {
    def apply(param:Int, range : Range) =
      BuiltInFunctionCall("get_local_id", param, range)

    def apply(param:Int) =
      BuiltInFunctionCall("get_local_id", param, ContinuousRange(0, get_local_size(param)))
  }

  object get_global_id {
    def apply(param:Int, range : Range) =
      BuiltInFunctionCall("get_global_id", param, range)

    def apply(param:Int) =
      BuiltInFunctionCall("get_global_id", param, ContinuousRange(0, get_global_size(param)))
  }

  object get_group_id {
    def apply(param:Int, range : Range) =
      BuiltInFunctionCall("get_group_id", param, range)

    def apply(param:Int) =
      BuiltInFunctionCall("get_group_id", param, ContinuousRange(0, get_num_groups(param)))

  }

  @silent("define classes/objects inside of package objects")
  trait FunctionHelper {
    type T
    type R
    type F = T => R
  }


  type `)=>`[TT, RR] = FunctionHelper { type T = TT; type R = RR }

  @silent("define classes/objects inside of package objects")
  sealed trait HList {
    def length: Int
    def toList: List[Any]
  }

  @silent("define classes/objects inside of package objects")
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
