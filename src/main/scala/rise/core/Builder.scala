package rise.core

import types._

trait Builder {
  def apply: DSL.ToBeTyped[Primitive] =
    throw new Exception("apply method must be overridden")
  def apply(e: DSL.ToBeTyped[Expr]): DSL.ToBeTyped[App] =
    DSL.app(DSL.toBeTyped(apply), e)
  def apply(n: Nat): DSL.ToBeTyped[DepApp[Nat, _]] =
    DSL.depApp(NatKind, DSL.toBeTyped(apply), n)
  def apply(dt: DataType): DSL.ToBeTyped[DepApp[DataType, _]] =
    DSL.depApp(DataKind, DSL.toBeTyped(apply), dt)
  def apply(a: AddressSpace): DSL.ToBeTyped[DepApp[AddressSpace, _]] =
    DSL.depApp(AddressSpaceKind, DSL.toBeTyped(apply), a)

  def unapply(arg: Expr): Boolean =
    throw new Exception("unapply method must be overridden")

  def primitive: Primitive =
    throw new Exception("primitive method must be overridden")
}
