package shine.DPIA

import rise.core.Expr
import rise.core.types.{Kind, KindName}
import rise.{core => r}

private object RiseExprAnnotated {
  sealed abstract class Expr[M <: MetaData] {
    val meta: M
  }

  final case class Identifier[M <: MetaData](
    name: String,
    override val meta: M
  ) extends Expr[M]

  final case class Lambda[M <: MetaData](
    x: Identifier[M],
    e: Expr[M],
    override val meta: M
  ) extends Expr[M]

  final case class App[M <: MetaData](
    f: Expr[M],
    e: Expr[M],
    override val meta: M
  ) extends Expr[M]

  final case class DepLambda[M <: MetaData, K <: Kind: KindName](
    x: K#I with Kind.Explicitness,
    e: Expr[M],
    override val meta: M
  ) extends Expr[M]

  final case class DepApp[M <: MetaData, K <: Kind](
    f: Expr[M],
    x: K#T,
    override val meta: M
  ) extends Expr[M]

  final case class Literal[M <: MetaData](
    override val meta: M
  ) extends Expr[M]

  final case class Primitive[M <: MetaData](
    override val meta: M
  ) extends Expr[M]
}
