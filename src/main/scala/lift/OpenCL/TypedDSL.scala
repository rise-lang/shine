package lift.OpenCL

import lift.core.TypedDSL._
import lift.core.{Expr, primitives => core}
import lift.OpenCL.primitives._
import lift.core.types.AddressSpaceKind

import scala.language.implicitConversions

object TypedDSL {
  object mapGlobal {
    def apply(): TDSL[MapGlobal] = tdsl(MapGlobal(0)())
    def apply[T <: Expr](e: TDSL[T]): TDSL[lift.core.App] = tdsl(MapGlobal(0)())(e)
    def apply(dim: Int): TDSL[MapGlobal] = tdsl(MapGlobal(dim)())
    implicit def toMapGlobal(m: MapGlobal.type): TDSL[MapGlobal] = tdsl(MapGlobal(0)())
  }

  object mapLocal {
    def apply(): TDSL[MapLocal] = tdsl(MapLocal(0)())
    def apply[T <: Expr](e: TDSL[T]): TDSL[lift.core.App] = tdsl(MapLocal(0)())(e)
    def apply(dim: Int): TDSL[MapLocal] = tdsl(MapLocal(dim)())
    implicit def toMapLocal(m: MapLocal.type): TDSL[MapLocal] = tdsl(MapLocal(0)())
  }

  object mapWorkGroup {
    def apply(): TDSL[MapWorkGroup] = tdsl(MapWorkGroup(0)())
    def apply[T <: Expr](e: TDSL[T]): TDSL[lift.core.App] = tdsl(MapWorkGroup(0)())(e)
    def apply(dim: Int): TDSL[MapWorkGroup] = tdsl(MapWorkGroup(dim)())
    implicit def toMapWorkGroup(m: MapWorkGroup.type): TDSL[MapWorkGroup] = tdsl(MapWorkGroup(0)())
  }

  def toMem: TDSL[ToMem] = tdsl(ToMem()())
  def toFun[A <: Expr, B <: Expr](to: TDSL[A], f: TDSL[B]): TDSL[lift.core.Lambda] = fun(x => to(f(x)))
  val toGlobal: TDSL[lift.core.DepApp[AddressSpaceKind]] = toMem(lift.core.types.AddressSpace.Global)
  def toGlobalFun[T <: Expr](f: TDSL[T]): TDSL[lift.core.Lambda] = toFun(toGlobal, f)
  val toLocal:TDSL[lift.core.DepApp[AddressSpaceKind]] = toMem(lift.core.types.AddressSpace.Local)
  def toLocalFun[T <: Expr](f: TDSL[T]): TDSL[lift.core.Lambda] = toFun(toLocal, f)
  val toPrivate: TDSL[lift.core.DepApp[AddressSpaceKind]] = toMem(lift.core.types.AddressSpace.Private)
  def toPrivateFun[T <: Expr](f: TDSL[T]): TDSL[lift.core.Lambda] = toFun(toPrivate, f)

  def oclReduceSeq: TDSL[OclReduceSeq] = tdsl(OclReduceSeq()())
  def oclReduceSeqUnroll: TDSL[OclReduceSeqUnroll] = tdsl(OclReduceSeqUnroll()())
  def oclIterate: TDSL[OclIterate] = tdsl(OclIterate()())
  def oclSlideSeq(rot: core.SlideSeq.Rotate): TDSL[OclSlideSeq] = tdsl(OclSlideSeq(rot)())
}
