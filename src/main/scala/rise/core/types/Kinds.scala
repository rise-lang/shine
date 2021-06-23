package rise.core.types

sealed trait Kind[+T, +I, +KI <: Kind.Identifier] {
  def name :String
}


object Kind {
  sealed trait Identifier { def name: String }
  case class IType(id : TypeIdentifier) extends Identifier { def name : String = id.name }
  case class IDataType(id : DataTypeIdentifier) extends Identifier { def name : String = id.name }
  case class INat(id : NatIdentifier) extends Identifier { def name : String = id.name }
  case class IAddressSpace(id : AddressSpaceIdentifier) extends Identifier { def name : String = id.name }
  case class INatToNat(id : NatToNatIdentifier) extends Identifier { def name : String = id.name }
  case class INatToData(id : NatToDataIdentifier) extends Identifier { def name : String = id.name }
  case class IMatrixLayout(id : MatrixLayoutIdentifier) extends Identifier { def name : String = id.name }
  case class IFragmentKind(id : FragmentKindIdentifier) extends Identifier { def name : String = id.name }
  case class INatCollection(id : NatCollectionIdentifier) extends Identifier { def name : String = id.name }

  def idName[T, I, KI <: Kind.Identifier](kind : Kind[T, I, KI], i : I) : String = kind match {
    case TypeKind => i.asInstanceOf[TypeIdentifier].name
    case DataKind => i.asInstanceOf[DataTypeIdentifier].name
    case NatKind => i.asInstanceOf[NatIdentifier].name
    case AddressSpaceKind => i.asInstanceOf[AddressSpaceIdentifier].name
    case NatToNatKind => i.asInstanceOf[NatToNatIdentifier].name
    case NatToDataKind => i.asInstanceOf[NatToDataIdentifier].name
    case NatCollectionKind => i.asInstanceOf[NatCollectionIdentifier].name
  }

  def toIdentifier[T, I, KI <: Kind.Identifier](kind : Kind[T, I, KI], i : I) : KI = kind match {
    case TypeKind => IType(i.asInstanceOf[TypeIdentifier]).asInstanceOf[KI]
    case DataKind => IDataType(i.asInstanceOf[DataTypeIdentifier]).asInstanceOf[KI]
    case NatKind => INat(i.asInstanceOf[NatIdentifier]).asInstanceOf[KI]
    case AddressSpaceKind => IAddressSpace(i.asInstanceOf[AddressSpaceIdentifier]).asInstanceOf[KI]
    case NatToNatKind => INatToNat(i.asInstanceOf[NatToNatIdentifier]).asInstanceOf[KI]
    case NatToDataKind => INatToData(i.asInstanceOf[NatToDataIdentifier]).asInstanceOf[KI]
    case NatCollectionKind => INatCollection(i.asInstanceOf[NatCollectionIdentifier]).asInstanceOf[KI]
  }

  def fromIdentifier[T, I, KI <: Kind.Identifier](kind : Kind[T, I, KI], i : KI) : I = kind match {
    case TypeKind => i.asInstanceOf[IType].id.asInstanceOf[I]
    case DataKind => i.asInstanceOf[IDataType].id.asInstanceOf[I]
    case NatKind => i.asInstanceOf[INat].id.asInstanceOf[I]
    case AddressSpaceKind => i.asInstanceOf[IAddressSpace].id.asInstanceOf[I]
    case NatToNatKind => i.asInstanceOf[INatToNat].id.asInstanceOf[I]
    case NatToDataKind => i.asInstanceOf[INatToData].id.asInstanceOf[I]
    case NatCollectionKind => i.asInstanceOf[INatCollection].id.asInstanceOf[I]
  }
}

case object TypeKind extends Kind[Type, TypeIdentifier, Kind.IType] {
  override def name: String = "type"
}

case object DataKind extends Kind[DataType, DataTypeIdentifier, Kind.IDataType] {
  override def name: String = "data"
}

case object NatKind extends Kind[Nat, NatIdentifier, Kind.INat] {
  override def name: String = "nat"
}

case object AddressSpaceKind extends Kind[AddressSpace, AddressSpaceIdentifier, Kind.IAddressSpace] {
  override def name: String = "addressSpace"
}

case object NatToNatKind extends Kind[NatToNat, NatToNatIdentifier, Kind.INatToNat] {
  override def name: String = "nat->nat"
}

case object NatToDataKind extends Kind[NatToData, NatToDataIdentifier, Kind.INatToData] {
  override def name: String = "nat->data"
}

case object NatCollectionKind extends Kind[NatCollection, NatCollectionIdentifier, Kind.INatCollection] {
  override def name: String = "nats"
}
