package rise.core.types

sealed trait Kind[+T, +I <: Kind.Identifier] {
  def name :String
}

object Kind {
  trait Identifier {
    def name: String
  }
}

case object TypeKind extends Kind[Type, TypeIdentifier] {
  override def name: String = "type"
}

case object DataKind extends Kind[DataType, DataTypeIdentifier] {
  override def name: String = "data"
}

case object NatKind extends Kind[Nat, NatIdentifier] {
  override def name: String = "nat"
}

case object AddressSpaceKind extends Kind[AddressSpace, AddressSpaceIdentifier] {
  override def name: String = "addressSpace"
}

case object NatToNatKind extends Kind[NatToNat, NatToNatIdentifier] {
  override def name: String = "nat->nat"
}

case object NatToDataKind extends Kind[NatToData, NatToDataIdentifier] {
  override def name: String = "nat->data"
}

case object NatCollectionKind extends Kind[NatCollection, NatCollectionIdentifier] {
  override def name: String = "nats"
}
