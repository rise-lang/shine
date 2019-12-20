package shine.DPIA.Types

import shine.DPIA
import shine.DPIA.NatIdentifier

sealed trait Kind {
  type T
  type I <: Kind.Identifier
}

object Kind {
  trait Identifier {
    def name: String
  }

  trait IdentifierMaker[K <: Kind] {
    def makeIdentifier(): K#I
  }

  implicit object DataTypeIdentifierMaker extends IdentifierMaker[DataKind] {
    override def makeIdentifier(): DataTypeIdentifier = DataTypeIdentifier(DPIA.freshName("dt"))
  }
  implicit object NatIdentifierMaker extends IdentifierMaker[NatKind] {
    override def makeIdentifier(): NatIdentifier = NatIdentifier(DPIA.freshName("n"))
  }
  implicit object AddrIdentifierMaker extends IdentifierMaker[AddressSpaceKind] {
    override def makeIdentifier(): AddressSpaceIdentifier = AddressSpaceIdentifier(DPIA.freshName("addr"))
  }
  implicit object AccessTypeIdentifierMaker extends IdentifierMaker[AccessKind] {
    override def makeIdentifier(): AccessTypeIdentifier = AccessTypeIdentifier(DPIA.freshName("access"))
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
  implicit val dataKN: KindName[DataKind] = new KindName[DataKind] {
    def get = "data"
  }
  implicit val addressSpaceKN: KindName[AddressSpaceKind] = new KindName[AddressSpaceKind] {
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