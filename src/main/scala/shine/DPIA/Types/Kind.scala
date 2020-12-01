package shine.DPIA.Types

import shine.DPIA
import shine.DPIA.fromRise.natIdentifier
import shine.DPIA.{Nat, NatIdentifier}

sealed trait Kind {
  type T
  type I <: T with Kind.Identifier
}

object Kind {
  trait Identifier {
    def name: String
  }

  trait IdentifierMaker[K <: Kind] {
    def makeIdentifier(): K#I
  }

  implicit object DataTypeIdentifierMaker
    extends IdentifierMaker[DataKind] {
    override def makeIdentifier(): DataTypeIdentifier =
      DataTypeIdentifier(DPIA.freshName("dt"))
  }
  implicit object NatIdentifierMaker
    extends IdentifierMaker[NatKind] {
    override def makeIdentifier(): NatIdentifier =
      NatIdentifier(DPIA.freshName("n"))
  }

  implicit object NatCollectionIdentifierMaker extends IdentifierMaker [NatCollectionKind] {
    override def makeIdentifier(): NatCollectionIdentifier =
      NatCollectionIdentifier(DPIA.freshName("ns"))
  }

  implicit object AddrIdentifierMaker
    extends IdentifierMaker[AddressSpaceKind] {
    override def makeIdentifier(): AddressSpaceIdentifier =
      AddressSpaceIdentifier(DPIA.freshName("addr"))
  }
  implicit object AccessTypeIdentifierMaker
    extends IdentifierMaker[AccessKind] {
    override def makeIdentifier(): AccessTypeIdentifier =
      AccessTypeIdentifier(DPIA.freshName("access"))
  }
}

sealed trait PhraseKind extends Kind {
  override type T = PhraseType
}

sealed trait DataKind extends Kind {
  override type T = DataType
  override type I = DataTypeIdentifier
}

sealed trait NatKind extends Kind {
  override type T = DPIA.Nat
  override type I = DPIA.NatIdentifier
}

sealed trait NatCollectionKind extends Kind {
  override type T = NatCollection
  override type I = NatCollectionIdentifier
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
  implicit val phraseKN: KindName[PhraseKind] = new KindName[PhraseKind] {
    def get = "phrase"
  }
  implicit val natKN: KindName[NatKind] = new KindName[NatKind] {
    def get = "nat"
  }
  implicit val natCollectionKN: KindName[NatCollectionKind] = new KindName[NatCollectionKind] {
    override def get: String = "natCollection"
  }
  implicit val dataKN: KindName[DataKind] = new KindName[DataKind] {
    def get = "data"
  }
  implicit val addressSpaceKN: KindName[AddressSpaceKind] =
    new KindName[AddressSpaceKind] {
    def get = "addressSpace"
  }
  implicit val accessKN: KindName[AccessKind] = new KindName[AccessKind] {
    def get = "access"
  }
  implicit val n2nKN: KindName[NatToNatKind] = new KindName[NatToNatKind] {
    def get = "nat->nat"
  }
  implicit val n2dtKN: KindName[NatToDataKind] = new KindName[NatToDataKind] {
    def get = "nat->data"
  }
}

trait KindReified[K <: Kind] {
  def tryFrom(x: Any): Option[K#T]
  def tryIdentifier(x:K#T): Option[K#I]
  def substitute(what:K#T, `for`:K#T, in:DataType): DataType
  def substituteNat(ae:Nat, `for`:Nat, in: K#T): K#T

  def visitNat(x:K#T, f: Nat => Nat):K#T
}

object KindReified {
  implicit val natKR: KindReified[NatKind] = new KindReified[NatKind] {
    override def tryFrom(x: Any): Option[Nat] = x match {
      case n: Nat => Some(n)
      case _ => None
    }

    override def tryIdentifier(x: Nat): Option[NatIdentifier] = x match {
      case x:NatIdentifier => Some(x)
      case x:rise.core.types.NatIdentifier => Some(natIdentifier(x))
      case _ => None
    }
    override def substitute(what: Nat, `for`: Nat, in: DataType): DataType = {
      DataType.substitute(what, `for`, in)
    }

    override def visitNat(x: Nat, f: Nat => Nat): Nat = f(x)

    override def substituteNat(ae: Nat, `for`: Nat, in: Nat): Nat = {
      `for` match {
        case ident: DPIA.NatIdentifier if ident == in => ae.asInstanceOf[NatIdentifier]
        case _ =>  in
      }
    }
  }

  implicit val natCollectionKR: KindReified[NatCollectionKind] = new KindReified[NatCollectionKind] {
    override def tryFrom(x: Any): Option[NatCollection] = x match {
      case x: NatCollection => Some(x)
      case _ => None
    }

    override def tryIdentifier(x: NatCollection): Option[NatCollectionIdentifier] = x match {
      case x: NatCollectionIdentifier => Some(x)
      case _ => None
    }

    override def visitNat(x: NatCollection, f: Nat => Nat): NatCollection = x

    override def substituteNat(ae: Nat, `for`: Nat, in: NatCollection): NatCollection = in

    override def substitute(what: NatCollection, `for`: NatCollection, in: DataType): DataType = in
  }
}