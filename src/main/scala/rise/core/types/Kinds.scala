package rise.core.types

import rise.core.freshName
import rise.core.types.DataType.DataTypeIdentifier

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
    case TypeKind => TypeKind.IDWrapper(i.asInstanceOf[TypeIdentifier])
    case DataKind => DataKind.IDWrapper(i.asInstanceOf[DataTypeIdentifier])
    case NatKind => NatKind.IDWrapper(i.asInstanceOf[NatIdentifier])
    case AddressSpaceKind => AddressSpaceKind.IDWrapper(i.asInstanceOf[AddressSpaceIdentifier])
    case NatToNatKind => NatToNatKind.IDWrapper(i.asInstanceOf[NatToNatIdentifier])
    case NatToDataKind => NatToDataKind.IDWrapper(i.asInstanceOf[NatToDataIdentifier])
    case NatCollectionKind => NatCollectionKind.IDWrapper(i.asInstanceOf[NatCollectionIdentifier])
    case MatrixLayoutKind => MatrixLayoutKind.IDWrapper(i.asInstanceOf[MatrixLayoutIdentifier])
    case FragmentKind => FragmentKind.IDWrapper(i.asInstanceOf[FragmentIdentifier])
    case AccessKind => AccessKind.IDWrapper(i.asInstanceOf[AccessIdentifier])
  }

  def fromIdentifier[T, I](kind: Kind[T, I], i: Kind.Identifier): I = kind match {
    case TypeKind => i.asInstanceOf[TypeKind.IDWrapper].id.asInstanceOf[I]
    case DataKind => i.asInstanceOf[DataKind.IDWrapper].id.asInstanceOf[I]
    case NatKind => i.asInstanceOf[NatKind.IDWrapper].id.asInstanceOf[I]
    case AddressSpaceKind => i.asInstanceOf[AddressSpaceKind.IDWrapper].id.asInstanceOf[I]
    case NatToNatKind => i.asInstanceOf[NatToNatKind.IDWrapper].id.asInstanceOf[I]
    case NatToDataKind => i.asInstanceOf[NatToDataKind.IDWrapper].id.asInstanceOf[I]
    case NatCollectionKind => i.asInstanceOf[NatCollectionKind.IDWrapper].id.asInstanceOf[I]
    case MatrixLayoutKind => i.asInstanceOf[MatrixLayoutKind.IDWrapper].id.asInstanceOf[I]
    case FragmentKind => i.asInstanceOf[FragmentKind.IDWrapper].id.asInstanceOf[I]
    case AccessKind => i.asInstanceOf[AccessKind.IDWrapper].id.asInstanceOf[I]
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

case object TypeKind extends Kind[ExprType, TypeIdentifier] {
  override def name: String = "type"
  override type KI = IDWrapper
  final case class IDWrapper(id : TypeIdentifier) extends Kind.Identifier { def name : String = id.name }
}

case object DataKind extends Kind[DataType, DataTypeIdentifier] {
  override def name: String = "data"
  override type KI = IDWrapper
  final case class IDWrapper(id : DataTypeIdentifier) extends Kind.Identifier { def name : String = id.name }
}

case object NatKind extends Kind[Nat, NatIdentifier] {
  override def name: String = "nat"
  override type KI = IDWrapper
  final case class IDWrapper(id : NatIdentifier) extends Kind.Identifier { def name : String = id.name }
}

case object AddressSpaceKind extends Kind[AddressSpace, AddressSpaceIdentifier] {
  override def name: String = "addressSpace"
  override type KI = IDWrapper
  final case class IDWrapper(id : AddressSpaceIdentifier) extends Kind.Identifier { def name : String = id.name }
}

case object NatToNatKind extends Kind[NatToNat, NatToNatIdentifier] {
  override def name: String = "nat->nat"
  override type KI = IDWrapper
  final case class IDWrapper(id : NatToNatIdentifier) extends Kind.Identifier { def name : String = id.name }
}

case object NatToDataKind extends Kind[NatToData, NatToDataIdentifier] {
  override def name: String = "nat->data"
  override type KI = IDWrapper
  final case class IDWrapper(id : NatToDataIdentifier) extends Kind.Identifier { def name : String = id.name }
}

case object NatCollectionKind extends Kind[NatCollection, NatCollectionIdentifier] {
  override def name: String = "nats"
  override type KI = IDWrapper
  final case class IDWrapper(id : NatCollectionIdentifier) extends Kind.Identifier { def name : String = id.name }
}

case object MatrixLayoutKind extends Kind[MatrixLayout, MatrixLayoutIdentifier] {
  override def name: String = "matrixLayout"
  override type KI = IDWrapper
  final case class IDWrapper(id : MatrixLayoutIdentifier) extends Kind.Identifier { def name : String = id.name }
}

case object FragmentKind extends Kind[Fragment, FragmentIdentifier] {
  override def name: String = "fragment"
  override type KI = IDWrapper
  final case class IDWrapper(id : FragmentIdentifier) extends Kind.Identifier { def name : String = id.name }
}

case object AccessKind extends Kind[Access, AccessIdentifier] {
  override def name: String = "access"
  override type KI = IDWrapper
  final case class IDWrapper(id: AccessIdentifier) extends Kind.Identifier { def name: String = id.name }
}
