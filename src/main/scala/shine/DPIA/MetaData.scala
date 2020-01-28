package shine.DPIA

import rise.core.types.{Kind, KindName, Type}
import shine.DPIA.Types.AccessType

trait MetaData

private object access {
  sealed trait AccessTypeAnnotation extends MetaData

  final case class FunType(
    in: AccessTypeAnnotation,
    out: AccessTypeAnnotation
  ) extends AccessTypeAnnotation

  final case class DepFunType[K <: Kind: KindName](
    x: K#I with Kind.Explicitness,
    a: AccessTypeAnnotation
  ) extends AccessTypeAnnotation

  final case class DataType(a: AccessType) extends AccessTypeAnnotation

  final case object PlaceHolder extends AccessTypeAnnotation
}
