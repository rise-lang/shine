package idealised.DPIA.Types

import idealised.DPIA
import idealised.DPIA.NatIdentifier

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

sealed trait DataKind extends Kind {
  override type T = DataType
  override type I = DataTypeIdentifier
}

sealed trait PhraseKind extends Kind {
  override type T = PhraseType
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