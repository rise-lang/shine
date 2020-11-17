package shine.DPIA.Types

sealed trait NatCollection

final case class NatCollectionIdentifier(name: String) extends NatCollection with Kind.Identifier
