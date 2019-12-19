package rise.OpenCL

import rise.core.DSL._
import rise.core.{Expr, primitives => core}
import rise.OpenCL.primitives._
import scala.language.implicitConversions

object DSL {
  object mapGlobal {
    def apply(): MapGlobal = MapGlobal(0)()
    def apply(e: Expr): Expr = MapGlobal(0)()(e)
    def apply(dim: Int): Expr = MapGlobal(dim)()
    implicit def toMapGlobal(m: MapGlobal.type): MapGlobal = MapGlobal(0)()
  }

  object mapLocal {
    def apply(): MapLocal = MapLocal(0)()
    def apply(e: Expr): Expr = MapLocal(0)()(e)
    def apply(dim: Int): Expr = MapLocal(dim)()
    implicit def toMapLocal(m: MapLocal.type): MapLocal = MapLocal(0)()
  }

  object mapWorkGroup {
    def apply(): MapWorkGroup = MapWorkGroup(0)()
    def apply(e: Expr): Expr = MapWorkGroup(0)()(e)
    def apply(dim: Int): Expr = MapWorkGroup(dim)()
    implicit def toMapWorkGroup(m: MapWorkGroup.type): MapWorkGroup = MapWorkGroup(0)()
  }

  def toMem: ToMem = ToMem()()
  def toFun(to: Expr, f: Expr): Expr = fun(x => to(f(x)))
  val toGlobal: Expr = toMem(rise.core.types.AddressSpace.Global)
  def toGlobalFun(f: Expr): Expr = toFun(toGlobal, f)
  val toLocal: Expr = toMem(rise.core.types.AddressSpace.Local)
  def toLocalFun(f: Expr): Expr = toFun(toLocal, f)
  val toPrivate: Expr = toMem(rise.core.types.AddressSpace.Private)
  def toPrivateFun(f: Expr): Expr = toFun(toPrivate, f)

  def oclReduceSeq: OclReduceSeq = OclReduceSeq()()
  def oclReduceSeqUnroll: OclReduceSeqUnroll = OclReduceSeqUnroll()()
  def oclIterate: OclIterate = OclIterate()()
  def oclSlideSeq(rot: core.SlideSeq.Rotate): OclSlideSeq = OclSlideSeq(rot)()
}
