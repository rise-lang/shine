package rise.core.dsl

import rise.core.dsl
import rise.core.exprs.{App, DepApp, Expr, Primitive}
import rise.core.types._

trait Builder {
  def apply: dsl.ToBeTyped[Primitive] =
    throw new Exception("apply method must be overridden")

  def apply(e: dsl.ToBeTyped[Expr]): dsl.ToBeTyped[App] =
    dsl.app(dsl.toBeTyped(apply), e)

  def apply(n: Nat): dsl.ToBeTyped[DepApp[NatKind]] =
    dsl.depApp[NatKind](dsl.toBeTyped(apply), n)

  def apply(dt: DataType): dsl.ToBeTyped[DepApp[DataKind]] =
    dsl.depApp[DataKind](dsl.toBeTyped(apply), dt)

  def apply(a: AddressSpace): dsl.ToBeTyped[DepApp[AddressSpaceKind]] =
    dsl.depApp[AddressSpaceKind](dsl.toBeTyped(apply), a)

  def unapply(arg: Expr): Boolean =
    throw new Exception("unapply method must be overridden")

  def primitive: Primitive =
    throw new Exception("primitive method must be overridden")
}
