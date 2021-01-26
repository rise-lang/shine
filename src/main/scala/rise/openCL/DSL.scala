package rise.openCL

import rise.core.DSL._
import rise.core.{Expr, Primitive}
import rise.core.types.AddressSpaceKind
import shine.OpenCL.{GlobalSize, LocalSize}

object DSL {
  object mapGlobal {
    def apply(): ToBeTyped[Primitive] = primitives.mapGlobal(0)
    def apply[T <: Expr](e: ToBeTyped[T]): ToBeTyped[rise.core.App] =
      primitives.mapGlobal(0)(e)
    def apply(dim: Int): ToBeTyped[Primitive] = primitives.mapGlobal(dim)
  }

  object mapLocal {
    def apply(): ToBeTyped[Primitive] = primitives.mapLocal(0)
    def apply[T <: Expr](e: ToBeTyped[T]): ToBeTyped[rise.core.App] =
      primitives.mapLocal(0)(e)
    def apply(dim: Int): ToBeTyped[Primitive] = primitives.mapLocal(dim)
  }

  object mapWorkGroup {
    def apply(): ToBeTyped[Primitive] = primitives.mapWorkGroup(0)
    def apply[T <: Expr](e: ToBeTyped[T]): ToBeTyped[rise.core.App] =
      primitives.mapWorkGroup(0)(e)
    def apply(dim: Int): ToBeTyped[Primitive] = primitives.mapWorkGroup(dim)
  }

  def toMem: ToBeTyped[Primitive] = primitives.oclToMem
  def toFun[A <: Expr, B <: Expr](
                                   to: ToBeTyped[A],
                                   f: ToBeTyped[B]
                                 ): ToBeTyped[rise.core.Lambda] = fun(x => to(f(x)))
  val toGlobal: ToBeTyped[rise.core.DepApp[AddressSpaceKind]] = toMem(
    rise.core.types.AddressSpace.Global
  )
  def toGlobalFun[T <: Expr](f: ToBeTyped[T]): ToBeTyped[rise.core.Lambda] =
    toFun(toGlobal, f)
  val toLocal: ToBeTyped[rise.core.DepApp[AddressSpaceKind]] = toMem(
    rise.core.types.AddressSpace.Local
  )
  def toLocalFun[T <: Expr](f: ToBeTyped[T]): ToBeTyped[rise.core.Lambda] =
    toFun(toLocal, f)
  val toPrivate: ToBeTyped[rise.core.DepApp[AddressSpaceKind]] = toMem(
    rise.core.types.AddressSpace.Private
  )
  def toPrivateFun[T <: Expr](f: ToBeTyped[T]): ToBeTyped[rise.core.Lambda] =
    toFun(toPrivate, f)

  def oclRun(ls: LocalSize, gs: GlobalSize): ToBeTyped[Expr] =
    primitives.oclRunP(ls.size.x)(ls.size.y)(ls.size.z)(gs.size.x)(gs.size.y)(gs.size.z)
}