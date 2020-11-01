package rise.opencl

import rise.core.dsl._
import rise.core.exprs.{primitives => _, _}
import rise.core.types.AddressSpaceKind

object dsl {
  object mapGlobal {
    def apply(): ToBeTyped[Primitive] = primitives.mapGlobal(0)
    def apply[T <: Expr](e: ToBeTyped[T]): ToBeTyped[App] =
      primitives.mapGlobal(0)(e)
    def apply(dim: Int): ToBeTyped[Primitive] = primitives.mapGlobal(dim)
  }

  object mapLocal {
    def apply(): ToBeTyped[Primitive] = primitives.mapLocal(0)
    def apply[T <: Expr](e: ToBeTyped[T]): ToBeTyped[App] =
      primitives.mapLocal(0)(e)
    def apply(dim: Int): ToBeTyped[Primitive] = primitives.mapLocal(dim)
  }

  object mapWorkGroup {
    def apply(): ToBeTyped[Primitive] = primitives.mapWorkGroup(0)
    def apply[T <: Expr](e: ToBeTyped[T]): ToBeTyped[App] =
      primitives.mapWorkGroup(0)(e)
    def apply(dim: Int): ToBeTyped[Primitive] = primitives.mapWorkGroup(dim)
  }

  def toMem: ToBeTyped[Primitive] = primitives.oclToMem
  def toFun[A <: Expr, B <: Expr](
                                   to: ToBeTyped[A],
                                   f: ToBeTyped[B]
                                 ): ToBeTyped[Lambda] = fun(x => to(f(x)))
  val toGlobal: ToBeTyped[DepApp[AddressSpaceKind]] = toMem(
    rise.core.types.AddressSpace.Global
  )
  def toGlobalFun[T <: Expr](f: ToBeTyped[T]): ToBeTyped[Lambda] =
    toFun(toGlobal, f)
  val toLocal: ToBeTyped[DepApp[AddressSpaceKind]] = toMem(
    rise.core.types.AddressSpace.Local
  )
  def toLocalFun[T <: Expr](f: ToBeTyped[T]): ToBeTyped[Lambda] =
    toFun(toLocal, f)
  val toPrivate: ToBeTyped[DepApp[AddressSpaceKind]] = toMem(
    rise.core.types.AddressSpace.Private
  )
  def toPrivateFun[T <: Expr](f: ToBeTyped[T]): ToBeTyped[Lambda] =
    toFun(toPrivate, f)
}
