package rise.core

import types._

trait Builder {
  def apply: TypedDSL.ToBeTyped[Primitive] =
    throw new Exception("apply method must be overridden")
  def apply(e: TypedDSL.ToBeTyped[Expr]): TypedDSL.ToBeTyped[App] =
    TypedDSL.app(TypedDSL.toBeTyped(apply), e)
  def apply(n: Nat): TypedDSL.ToBeTyped[DepApp[NatKind]] =
    TypedDSL.depApp[NatKind](TypedDSL.toBeTyped(apply), n)
  def apply(dt: DataType): TypedDSL.ToBeTyped[DepApp[DataKind]] =
    TypedDSL.depApp[DataKind](TypedDSL.toBeTyped(apply), dt)
  def apply(a: AddressSpace): TypedDSL.ToBeTyped[DepApp[AddressSpaceKind]] =
    TypedDSL.depApp[AddressSpaceKind](TypedDSL.toBeTyped(apply), a)

  def unapply(arg: Expr): Boolean =
    throw new Exception("unapply method must be overridden")

  def primitive: Primitive =
    throw new Exception("primitive method must be overridden")
}
