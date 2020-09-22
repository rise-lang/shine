package rise.core

import types._

trait Builder {
  def apply: TypedDSL.TDSL[Primitive] =
    throw new Exception("apply method must be overridden")
  def apply(e: TypedDSL.TDSL[Expr]): TypedDSL.TDSL[App] =
    TypedDSL.app(TypedDSL.toTDSL(apply), e)
  def apply(n: Nat): TypedDSL.TDSL[DepApp[NatKind]] =
    TypedDSL.depApp[NatKind](TypedDSL.toTDSL(apply), n)
  def apply(dt: DataType): TypedDSL.TDSL[DepApp[DataKind]] =
    TypedDSL.depApp[DataKind](TypedDSL.toTDSL(apply), dt)
  def apply(a: AddressSpace): TypedDSL.TDSL[DepApp[AddressSpaceKind]] =
    TypedDSL.depApp[AddressSpaceKind](TypedDSL.toTDSL(apply), a)

  def unapply(arg: Expr): Boolean =
    throw new Exception("unapply method must be overridden")

  def primitive: Primitive =
    throw new Exception("primitive method must be overridden")
}
