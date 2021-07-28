package rise.core.types

import rise.core.freshName

sealed trait Kind[+T, +I] {
  def name :String
  type KI <: Kind.Identifier
}

object Kind {
  sealed trait Identifier { def name: String }

  def idName[T, I](kind: Kind[T, I], i: I): String = kind match {
    case TypeKind => i.asInstanceOf[TypeIdentifier].name
    case DataKind => i.asInstanceOf[DataTypeIdentifier].name
    case NatKind => i.asInstanceOf[NatIdentifier].name
    case AddressSpaceKind => i.asInstanceOf[AddressSpaceIdentifier].name
    case NatToNatKind => i.asInstanceOf[NatToNatIdentifier].name
    case NatToDataKind => i.asInstanceOf[NatToDataIdentifier].name
    case NatCollectionKind => i.asInstanceOf[NatCollectionIdentifier].name
    case MatrixLayoutKind => i.asInstanceOf[MatrixLayoutIdentifier].name
    case FragmentKind => i.asInstanceOf[FragmentIdentifier].name
    case AccessKind => i.asInstanceOf[AccessIdentifier].name
  }

  def toIdentifier[T, I](kind: Kind[T, I], i: I): Kind.Identifier = kind match {
    case TypeKind => TypeKind.Identifier(i.asInstanceOf[TypeIdentifier])
    case DataKind => DataKind.Identifier(i.asInstanceOf[DataTypeIdentifier])
    case NatKind => NatKind.Identifier(i.asInstanceOf[NatIdentifier])
    case AddressSpaceKind => AddressSpaceKind.Identifier(i.asInstanceOf[AddressSpaceIdentifier])
    case NatToNatKind => NatToNatKind.Identifier(i.asInstanceOf[NatToNatIdentifier])
    case NatToDataKind => NatToDataKind.Identifier(i.asInstanceOf[NatToDataIdentifier])
    case NatCollectionKind => NatCollectionKind.Identifier(i.asInstanceOf[NatCollectionIdentifier])
    case MatrixLayoutKind => MatrixLayoutKind.Identifier(i.asInstanceOf[MatrixLayoutIdentifier])
    case FragmentKind => FragmentKind.Identifier(i.asInstanceOf[FragmentIdentifier])
    case AccessKind => AccessKind.Identifier(i.asInstanceOf[AccessIdentifier])
  }

  def fromIdentifier[T, I](kind: Kind[T, I], i: Kind.Identifier): I = kind match {
    case TypeKind => i.asInstanceOf[TypeKind.Identifier].id.asInstanceOf[I]
    case DataKind => i.asInstanceOf[DataKind.Identifier].id.asInstanceOf[I]
    case NatKind => i.asInstanceOf[NatKind.Identifier].id.asInstanceOf[I]
    case AddressSpaceKind => i.asInstanceOf[AddressSpaceKind.Identifier].id.asInstanceOf[I]
    case NatToNatKind => i.asInstanceOf[NatToNatKind.Identifier].id.asInstanceOf[I]
    case NatToDataKind => i.asInstanceOf[NatToDataKind.Identifier].id.asInstanceOf[I]
    case NatCollectionKind => i.asInstanceOf[NatCollectionKind.Identifier].id.asInstanceOf[I]
    case MatrixLayoutKind => i.asInstanceOf[MatrixLayoutKind.Identifier].id.asInstanceOf[I]
    case FragmentKind => i.asInstanceOf[FragmentKind.Identifier].id.asInstanceOf[I]
    case AccessKind => i.asInstanceOf[AccessKind.Identifier].id.asInstanceOf[I]
  }

  def makeIdentifier[T, I](kind: Kind[T, I]): I = kind match {
    case TypeKind => TypeIdentifier(freshName("t")).asInstanceOf[I]
    case DataKind => DataTypeIdentifier(freshName("dt")).asInstanceOf[I]
    case NatKind => NatIdentifier(freshName("n")).asInstanceOf[I]
    case AddressSpaceKind => AddressSpaceIdentifier(freshName("addr")).asInstanceOf[I]
    case NatToNatKind => NatToNatIdentifier(freshName("n2n")).asInstanceOf[I]
    case NatToDataKind => NatToDataIdentifier(freshName("n2d")).asInstanceOf[I]
    case NatCollectionKind => NatCollectionIdentifier(freshName("ns")).asInstanceOf[I]
    case MatrixLayoutKind => MatrixLayoutIdentifier(freshName("m")).asInstanceOf[I]
    case FragmentKind => FragmentIdentifier(freshName("f")).asInstanceOf[I]
    case AccessKind => AccessIdentifier(freshName("access")).asInstanceOf[I]
  }
}

case object TypeKind extends Kind[Type, TypeIdentifier] {
  override def name: String = "type"
  override type KI = Identifier
  final case class Identifier(id : TypeIdentifier) extends Kind.Identifier { def name : String = id.name }
}

case object DataKind extends Kind[DataType, DataTypeIdentifier] {
  override def name: String = "data"
  override type KI = Identifier
  final case class Identifier(id : DataTypeIdentifier) extends Kind.Identifier { def name : String = id.name }
}

case object NatKind extends Kind[Nat, NatIdentifier] {
  override def name: String = "nat"
  override type KI = Identifier
  final case class Identifier(id : NatIdentifier) extends Kind.Identifier { def name : String = id.name }
}

case object AddressSpaceKind extends Kind[AddressSpace, AddressSpaceIdentifier] {
  override def name: String = "addressSpace"
  override type KI = Identifier
  final case class Identifier(id : AddressSpaceIdentifier) extends Kind.Identifier { def name : String = id.name }
}

case object NatToNatKind extends Kind[NatToNat, NatToNatIdentifier] {
  override def name: String = "nat->nat"
  override type KI = Identifier
  final case class Identifier(id : NatToNatIdentifier) extends Kind.Identifier { def name : String = id.name }
}

case object NatToDataKind extends Kind[NatToData, NatToDataIdentifier] {
  override def name: String = "nat->data"
  override type KI = Identifier
  final case class Identifier(id : NatToDataIdentifier) extends Kind.Identifier { def name : String = id.name }
}

case object NatCollectionKind extends Kind[NatCollection, NatCollectionIdentifier] {
  override def name: String = "nats"
  override type KI = Identifier
  final case class Identifier(id : NatCollectionIdentifier) extends Kind.Identifier { def name : String = id.name }
}

case object MatrixLayoutKind extends Kind[MatrixLayout, MatrixLayoutIdentifier] {
  override def name: String = "matrixLayout"
  override type KI = Identifier
  final case class Identifier(id : MatrixLayoutIdentifier) extends Kind.Identifier { def name : String = id.name }
}

case object FragmentKind extends Kind[Fragment, FragmentIdentifier] {
  override def name: String = "fragment"
  override type KI = Identifier
  final case class Identifier(id : FragmentIdentifier) extends Kind.Identifier { def name : String = id.name }
}

case object AccessKind extends Kind[Access, AccessIdentifier] {
  override def name: String = "access"
  override type KI = Identifier
  final case class Identifier(id: AccessIdentifier) extends Kind.Identifier { def name: String = id.name }
}
