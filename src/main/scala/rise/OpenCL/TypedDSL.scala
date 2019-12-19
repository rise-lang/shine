package rise.OpenCL

import rise.core.TypedDSL._
import rise.core.{Expr, primitives => core}
import rise.OpenCL.primitives._
import rise.core.types.AddressSpaceKind

import scala.language.implicitConversions

object TypedDSL {
  object mapGlobal {
    def apply(): TDSL[MapGlobal] = toTDSL(MapGlobal(0)())
    def apply[T <: Expr](e: TDSL[T]): TDSL[rise.core.App] = toTDSL(MapGlobal(0)())(e)
    def apply(dim: Int): TDSL[MapGlobal] = toTDSL(MapGlobal(dim)())
    implicit def toMapGlobal(m: MapGlobal.type): TDSL[MapGlobal] = toTDSL(MapGlobal(0)())
  }

  object mapLocal {
    def apply(): TDSL[MapLocal] = toTDSL(MapLocal(0)())
    def apply[T <: Expr](e: TDSL[T]): TDSL[rise.core.App] = toTDSL(MapLocal(0)())(e)
    def apply(dim: Int): TDSL[MapLocal] = toTDSL(MapLocal(dim)())
    implicit def toMapLocal(m: MapLocal.type): TDSL[MapLocal] = toTDSL(MapLocal(0)())
  }

  object mapWorkGroup {
    def apply(): TDSL[MapWorkGroup] = toTDSL(MapWorkGroup(0)())
    def apply[T <: Expr](e: TDSL[T]): TDSL[rise.core.App] = toTDSL(MapWorkGroup(0)())(e)
    def apply(dim: Int): TDSL[MapWorkGroup] = toTDSL(MapWorkGroup(dim)())
    implicit def toMapWorkGroup(m: MapWorkGroup.type): TDSL[MapWorkGroup] = toTDSL(MapWorkGroup(0)())
  }

  def toMem: TDSL[ToMem] = toTDSL(ToMem()())
  def toFun[A <: Expr, B <: Expr](to: TDSL[A], f: TDSL[B]): TDSL[rise.core.Lambda] = fun(x => to(f(x)))
  val toGlobal: TDSL[rise.core.DepApp[AddressSpaceKind]] = toMem(rise.core.types.AddressSpace.Global)
  def toGlobalFun[T <: Expr](f: TDSL[T]): TDSL[rise.core.Lambda] = toFun(toGlobal, f)
  val toLocal:TDSL[rise.core.DepApp[AddressSpaceKind]] = toMem(rise.core.types.AddressSpace.Local)
  def toLocalFun[T <: Expr](f: TDSL[T]): TDSL[rise.core.Lambda] = toFun(toLocal, f)
  val toPrivate: TDSL[rise.core.DepApp[AddressSpaceKind]] = toMem(rise.core.types.AddressSpace.Private)
  def toPrivateFun[T <: Expr](f: TDSL[T]): TDSL[rise.core.Lambda] = toFun(toPrivate, f)

  def oclReduceSeq: TDSL[OclReduceSeq] = toTDSL(OclReduceSeq()())
  def oclReduceSeqUnroll: TDSL[OclReduceSeqUnroll] = toTDSL(OclReduceSeqUnroll()())
  def oclIterate: TDSL[OclIterate] = toTDSL(OclIterate()())
  def oclSlideSeq(rot: core.SlideSeq.Rotate): TDSL[OclSlideSeq] = toTDSL(OclSlideSeq(rot)())
}
