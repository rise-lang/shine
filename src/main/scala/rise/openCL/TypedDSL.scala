package rise.openCL

import rise.core.TypedDSL._
import rise.core.{Expr, Primitive}
import rise.core.types.AddressSpaceKind

object TypedDSL {
  object mapGlobal {
    def apply(): TDSL[Primitive] = primitives.mapGlobal(0)
    def apply[T <: Expr](e: TDSL[T]): TDSL[rise.core.App] =
      primitives.mapGlobal(0)(e)
    def apply(dim: Int): TDSL[Primitive] = primitives.mapGlobal(dim)
  }

  object mapLocal {
    def apply(): TDSL[Primitive] = primitives.mapLocal(0)
    def apply[T <: Expr](e: TDSL[T]): TDSL[rise.core.App] =
      primitives.mapLocal(0)(e)
    def apply(dim: Int): TDSL[Primitive] = primitives.mapLocal(dim)
  }

  object mapWorkGroup {
    def apply(): TDSL[Primitive] = primitives.mapWorkGroup(0)
    def apply[T <: Expr](e: TDSL[T]): TDSL[rise.core.App] =
      primitives.mapWorkGroup(0)(e)
    def apply(dim: Int): TDSL[Primitive] = primitives.mapWorkGroup(dim)
  }

  def toMem: TDSL[Primitive] = primitives.oclToMem
  def toFun[A <: Expr, B <: Expr](
                                   to: TDSL[A],
                                   f: TDSL[B]
                                 ): TDSL[rise.core.Lambda] = fun(x => to(f(x)))
  val toGlobal: TDSL[rise.core.DepApp[AddressSpaceKind]] = toMem(
    rise.core.types.AddressSpace.Global
  )
  def toGlobalFun[T <: Expr](f: TDSL[T]): TDSL[rise.core.Lambda] =
    toFun(toGlobal, f)
  val toLocal: TDSL[rise.core.DepApp[AddressSpaceKind]] = toMem(
    rise.core.types.AddressSpace.Local
  )
  def toLocalFun[T <: Expr](f: TDSL[T]): TDSL[rise.core.Lambda] =
    toFun(toLocal, f)
  val toPrivate: TDSL[rise.core.DepApp[AddressSpaceKind]] = toMem(
    rise.core.types.AddressSpace.Private
  )
  def toPrivateFun[T <: Expr](f: TDSL[T]): TDSL[rise.core.Lambda] =
    toFun(toPrivate, f)
}
