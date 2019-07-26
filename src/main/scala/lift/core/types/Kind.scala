package lift.core.types

import lift.core

sealed trait Kind {
  type T
  type I <: Kind.Identifier with T
}

object Kind {
  trait Identifier {
    def name: String
  }
}

sealed trait TypeKind extends Kind {
  override type T = Type
}

sealed trait DataKind extends Kind {
  override type T = DataType
  override type I = DataTypeIdentifier
}

sealed trait NatKind extends Kind {
  override type T = core.Nat
  override type I = core.NatIdentifier
}

sealed trait AddressSpaceKind extends Kind {
  override type T = AddressSpace
  override type I = AddressSpaceIdentifier
}

sealed trait AccessKind extends Kind {
  override type T = AccessType
  override type I = AccessTypeIdentifier
}

sealed trait NatToNatKind extends Kind {
  override type T = NatToNat
  override type I = NatToNatIdentifier
}

sealed trait NatToDataKind extends Kind {
  override type T = NatToData
  override type I = NatToDataIdentifier
}

trait KindName[K <: Kind] {
  def get: String
}

object KindName {
  implicit val typeKN: KindName[TypeKind] = new KindName[TypeKind] {
    def get = "type"
  }
  implicit val dataKN: KindName[DataKind] = new KindName[DataKind] {
    def get = "data"
  }
  implicit val natKN: KindName[NatKind] = new KindName[NatKind] {
    def get = "nat"
  }
  implicit val addressSpaceKN: KindName[AddressSpaceKind] = new KindName[AddressSpaceKind] {
    def get = "addressSpace"
  }
  implicit val AccessTypeKN: KindName[AccessKind] = new KindName[AccessKind] {
    def get = "access"
  }
  implicit val n2nKN: KindName[NatToNatKind] = new KindName[NatToNatKind] {
    def get = "nat->nat"
  }
  implicit val n2dtKN: KindName[NatToDataKind] = new KindName[NatToDataKind] {
    def get = "nat->data"
  }
}
