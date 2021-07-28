//package shine.DPIA.Types
//
//import shine.DPIA
//import shine.DPIA.NatIdentifier
//
//sealed trait Kind[+T, +I, +KI <: Kind.Identifier] {
//  def name: String
//  def makeIdentifier: I
//}
//
//object Kind {
//  sealed trait Identifier { def name: String }
//  case class IDataType(id : DataTypeIdentifier) extends Identifier { def name : String = id.name }
//  case class INat(id : NatIdentifier) extends Identifier { def name : String = id.name }
//  case class IAddressSpace(id : AddressSpaceIdentifier) extends Identifier { def name : String = id.name }
//  case class IAccessType(id : AccessIdentifier) extends Identifier { def name : String = id.name }
//  case class INatToNat(id : NatToNatIdentifier) extends Identifier { def name : String = id.name }
//  case class INatToData(id : NatToDataIdentifier) extends Identifier { def name : String = id.name }
//
//  def idName[T, I, KI <: Kind.Identifier](kind : Kind[T, I, KI], i : I) : String = kind match {
//    case DataKind => i.asInstanceOf[DataTypeIdentifier].name
//    case NatKind => i.asInstanceOf[NatIdentifier].name
//    case AddressSpaceKind => i.asInstanceOf[AddressSpaceIdentifier].name
//    case AccessKind => i.asInstanceOf[AccessIdentifier].name
//    case NatToNatKind => i.asInstanceOf[NatToNatIdentifier].name
//    case NatToDataKind => i.asInstanceOf[NatToDataIdentifier].name
//  }
//
//  def toIdentifier[T, I, KI <: Kind.Identifier](kind : Kind[T, I, KI], i : I) : KI = kind match {
//    case DataKind => IDataType(i.asInstanceOf[DataTypeIdentifier]).asInstanceOf[KI]
//    case NatKind => INat(i.asInstanceOf[NatIdentifier]).asInstanceOf[KI]
//    case AddressSpaceKind => IAddressSpace(i.asInstanceOf[AddressSpaceIdentifier]).asInstanceOf[KI]
//    case AccessKind => IAccessType(i.asInstanceOf[AccessIdentifier]).asInstanceOf[KI]
//    case NatToNatKind => INatToNat(i.asInstanceOf[NatToNatIdentifier]).asInstanceOf[KI]
//    case NatToDataKind => INatToData(i.asInstanceOf[NatToDataIdentifier]).asInstanceOf[KI]
//  }
//
//  def fromIdentifier[T, I, KI <: Kind.Identifier](kind : Kind[T, I, KI], i : KI) : I = kind match {
//    case DataKind => i.asInstanceOf[IDataType].id.asInstanceOf[I]
//    case NatKind => i.asInstanceOf[INat].id.asInstanceOf[I]
//    case AddressSpaceKind => i.asInstanceOf[IAddressSpace].id.asInstanceOf[I]
//    case AccessKind => i.asInstanceOf[IAccessType].id.asInstanceOf[I]
//    case NatToNatKind => i.asInstanceOf[INatToNat].id.asInstanceOf[I]
//    case NatToDataKind => i.asInstanceOf[INatToData].id.asInstanceOf[I]
//  }
//}
//
//case object DataKind extends Kind[DataType, DataTypeIdentifier, Kind.IDataType] {
//  override def name: String = "data"
//  override def makeIdentifier: DataTypeIdentifier = DataTypeIdentifier(DPIA.freshName("dt"))
//}
//
//case object NatKind extends Kind[DPIA.Nat, DPIA.NatIdentifier, Kind.INat] {
//  override def name: String = "nat"
//  override def makeIdentifier: NatIdentifier = NatIdentifier(DPIA.freshName("n"))
//}
//
//case object AddressSpaceKind extends Kind[AddressSpace, AddressSpaceIdentifier, Kind.IAddressSpace] {
//  override def name: String = "addressSpace"
//  override def makeIdentifier: AddressSpaceIdentifier = AddressSpaceIdentifier(DPIA.freshName("addr"))
//}
//
//case object AccessKind extends Kind[Access, AccessIdentifier, Kind.IAccessType] {
//  override def name: String = "access"
//  override def makeIdentifier: AccessIdentifier = AccessIdentifier(DPIA.freshName("access"))
//}
//
//case object NatToNatKind extends Kind[NatToNat, NatToNatIdentifier, Kind.INatToNat] {
//  override def name: String = "nat->nat"
//  override def makeIdentifier: NatToNatIdentifier = NatToNatIdentifier(DPIA.freshName("n2n"))
//}
//
//case object NatToDataKind extends Kind[NatToData, NatToDataIdentifier, Kind.INatToData] {
//  override def name: String = "nat->data"
//  override def makeIdentifier: NatToDataIdentifier = NatToDataIdentifier(DPIA.freshName("n2d"))
//}
