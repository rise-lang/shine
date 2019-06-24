package lift.core.types

sealed trait Kind {
  type T
  type I <: Kind.Identifier
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
  override type T = lift.core.Nat
  override type I = lift.core.NatIdentifier
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
}
