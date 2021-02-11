package rise.Cuda

import rise.core.DSL._
import rise.core.semantics.HalfData
import rise.core.{DepApp, Expr, Literal, Primitive}
import rise.core.types.Nat

object TypedDSL {
  object mapBlock {
    def apply(): ToBeTyped[Primitive] = primitives.mapBlock('x')
    def apply[T <: Expr](e: ToBeTyped[T]): ToBeTyped[rise.core.App] = primitives.mapBlock('x')(e)
    def apply(dim: Char): ToBeTyped[Primitive] = primitives.mapBlock(dim)
  }

  object mapGlobal {
    def apply(): ToBeTyped[Primitive] = primitives.mapGlobal('x')
    def apply[T <: Expr](e: ToBeTyped[T]): ToBeTyped[rise.core.App] = primitives.mapGlobal('x')(e)
    def apply(dim: Char): ToBeTyped[Primitive] = primitives.mapGlobal(dim)
  }

  object mapThreads {
    def apply(): ToBeTyped[Primitive] = primitives.mapThreads('x')
    def apply[T <: Expr](e: ToBeTyped[T]): ToBeTyped[rise.core.App] = primitives.mapThreads('x')(e)
    def apply(dim: Char): ToBeTyped[Primitive] = primitives.mapThreads(dim)
  }

  object mapWarp {
    def apply(): ToBeTyped[Primitive] = primitives.mapWarp('x')
    def apply[T <: Expr](e: ToBeTyped[T]): ToBeTyped[rise.core.App] = primitives.mapWarp('x')(e)
    def apply(dim: Char): ToBeTyped[Primitive] = primitives.mapWarp(dim)
  }

  object mapLane {
    def apply(): ToBeTyped[Primitive] = primitives.mapLane('x')
    def apply[T <: Expr](e: ToBeTyped[T]): ToBeTyped[rise.core.App] = primitives.mapLane('x')(e)
    def apply(dim: Char): ToBeTyped[Primitive] = primitives.mapLane(dim)
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
