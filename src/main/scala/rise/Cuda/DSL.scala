package rise.Cuda

import rise.core.DSL._
import rise.core.types.Nat
import rise.core.{Expr, Primitive}

object DSL {
  object mapBlock {
    def apply(): ToBeTyped[Primitive] = primitives.mapBlock(0)
    def apply[T <: Expr](e: ToBeTyped[T]): ToBeTyped[rise.core.App] = primitives.mapBlock(0)(e)
    def apply(dim: Int): ToBeTyped[Primitive] = primitives.mapBlock(dim)
  }

  object mapGlobal {
    def apply(): ToBeTyped[Primitive] = primitives.mapGlobal(0)
    def apply[T <: Expr](e: ToBeTyped[T]): ToBeTyped[rise.core.App] = primitives.mapGlobal(0)(e)
    def apply(dim: Int): ToBeTyped[Primitive] = primitives.mapGlobal(dim)
  }

  object mapThreads {
    def apply(): ToBeTyped[Primitive] = primitives.mapThreads(0)
    def apply[T <: Expr](e: ToBeTyped[T]): ToBeTyped[rise.core.App] = primitives.mapThreads(0)(e)
    def apply(dim: Int): ToBeTyped[Primitive] = primitives.mapThreads(dim)
  }

  object mapWarp {
    def apply(): ToBeTyped[Primitive] = primitives.mapWarp(0)
    def apply[T <: Expr](e: ToBeTyped[T]): ToBeTyped[rise.core.App] = primitives.mapWarp(0)(e)
    def apply(dim: Int): ToBeTyped[Primitive] = primitives.mapWarp(dim)
  }

  object mapLane {
    def apply(): ToBeTyped[Primitive] = primitives.mapLane(0)
    def apply[T <: Expr](e: ToBeTyped[T]): ToBeTyped[rise.core.App] = primitives.mapLane(0)(e)
    def apply(dim: Int): ToBeTyped[Primitive] = primitives.mapLane(dim)
  }

  object tensorMMA {
    def apply[T <: Expr](a: ToBeTyped[T]): ToBeTyped[rise.core.App] = primitives.tensorMMA(a)
    def apply[T <: Expr, U <: Expr](a: ToBeTyped[T], b: ToBeTyped[U]): ToBeTyped[rise.core.App] = primitives.tensorMMA(a)(b)
    def apply[T <: Expr, U <: Expr, V <: Expr](a: ToBeTyped[T], b: ToBeTyped[U], c: ToBeTyped[V]): ToBeTyped[rise.core.App] = primitives.tensorMMA(a)(b)(c)
  }

  object toSharedMemoryShift {
    def apply(shift: Nat): Expr = primitives.toSharedMemoryShift(shift)
    def apply(shift: Nat, array: Expr): Expr = primitives.toSharedMemoryShift(shift)(array)
  }
}
