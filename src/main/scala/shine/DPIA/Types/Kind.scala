package shine.DPIA.Types

import shine.DPIA
import shine.DPIA.NatIdentifier

sealed trait Kind[+T, +I <: Kind.Identifier] {
  def name: String
  def makeIdentifier: I
}

object Kind {
  trait Identifier {
    def name: String
  }
}

case object PhraseKind extends Kind[PhraseType, Kind.Identifier] {
  override def name: String = "phrase"
  override def makeIdentifier: Kind.Identifier = ???
}

case object DataKind extends Kind[DataType, DataTypeIdentifier] {
  override def name: String = "data"
  override def makeIdentifier: DataTypeIdentifier = DataTypeIdentifier(DPIA.freshName("dt"))
}

case object NatKind extends Kind[DPIA.Nat, DPIA.NatIdentifier] {
  override def name: String = "nat"
  override def makeIdentifier: NatIdentifier = NatIdentifier(DPIA.freshName("n"))
}

case object AddressSpaceKind extends Kind[AddressSpace, AddressSpaceIdentifier] {
  override def name: String = "addressSpace"
  override def makeIdentifier: AddressSpaceIdentifier = AddressSpaceIdentifier(DPIA.freshName("addr"))
}

case object AccessKind extends Kind[AccessType, AccessTypeIdentifier] {
  override def name: String = "access"
  override def makeIdentifier: AccessTypeIdentifier = AccessTypeIdentifier(DPIA.freshName("access"))
}

case object NatToNatKind extends Kind[NatToNat, NatToNatIdentifier] {
  override def name: String = "nat->nat"
  override def makeIdentifier: NatToNatIdentifier = NatToNatIdentifier(DPIA.freshName("n2n"))
}

case object NatToDataKind extends Kind[NatToData, NatToDataIdentifier] {
  override def name: String = "nat->data"
  override def makeIdentifier: NatToDataIdentifier = NatToDataIdentifier(DPIA.freshName("n2d"))
}
