package shine.DPIA

import rise.core.types.{Kind, KindName}
import shine.DPIA.Types.PhraseType

private object RiseExprAnnotated {
  sealed abstract class Expr {
    val pt: PhraseType
  }

  final case class Identifier(
    name: String,
    override val pt: PhraseType
  ) extends Expr

  final case class Lambda(
    x: Identifier,
    e: Expr,
    override val pt: PhraseType
  ) extends Expr

  final case class App(
    f: Expr,
    e: Expr,
    override val pt: PhraseType
  ) extends Expr

  final case class DepLambda[K <: Kind: KindName](
    x: K#I with Kind.Explicitness,
    e: Expr,
    override val pt: PhraseType
  ) extends Expr

  final case class DepApp[K <: Kind](
    f: Expr,
    x: K#T,
    override val pt: PhraseType
  ) extends Expr

  final case class Literal(
    override val pt: PhraseType
  ) extends Expr

  final case class Primitive(
    override val pt: PhraseType
  ) extends Expr
}
