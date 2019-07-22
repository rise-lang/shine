package lift.core.types

import lift.core

sealed trait Kind {
  type T
  type I <: Kind.Identifier
}

object Kind {
  trait Identifier {
    def name: String
  }

  def formatKindName(s: String): String =
    s.dropWhile(_!='$').drop(1).takeWhile(_!='$') match {
      case "NatIdentifier" => "nat"
      case "DataTypeIdentifier" => "data"
      case "NatToNatIdentifier" => "nat->nat"
      case "NatToDataIdentifier" => "nat->data"
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
  implicit val typeKindName: KindName[TypeKind] = new KindName[TypeKind] {
    def get = "type"
  }
  implicit val dataKindName: KindName[DataKind] = new KindName[DataKind] {
    def get = "data"
  }
  implicit val natKindName: KindName[NatKind] = new KindName[NatKind] {
    def get = "nat"
  }
  implicit val addressSpaceKindName: KindName[AddressSpaceKind] = new KindName[AddressSpaceKind] {
    def get = "addressSpace"
  }
}
