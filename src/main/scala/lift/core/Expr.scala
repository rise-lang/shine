package lift.core

import lift.core.types._
import lift.core.DSL._

sealed abstract class Expr {
  val t: Type
  def setType(t: Type): Expr
}

final case class Identifier(name: String)
                           (override val t: Type = TypePlaceholder) extends Expr {
  override def toString: String = ShowLiftCompactTrack(this)
  override def setType(t: Type): Identifier = this.copy(name)(t)
}

final case class Lambda(x: Identifier, e: Expr)
                       (override val t: Type = TypePlaceholder) extends Expr {
  override def toString: String = ShowLiftCompactTrack(this)

  override def equals(obj: Any): Boolean = obj match {
    case other: Lambda => e == lifting.liftFunExpr(other).value(x)
    case _ => false
  }

  override def setType(t: Type): Lambda = this.copy(x, e)(t)
}

final case class App(f: Expr, e: Expr)
                    (override val t: Type = TypePlaceholder) extends Expr {
  override def toString: String = ShowLiftCompactTrack(this)

  override def setType(t: Type): App = this.copy(f, e)(t)
}

final case class DepLambda[K <: Kind](x: K#I, e: Expr)
                                     (override val t: Type = TypePlaceholder)
                                     (implicit val kn: KindName[K]) extends Expr {
  override def toString: String = ShowLiftCompactTrack(this)

  override def equals(obj: Any): Boolean = obj match {
    case other: DepLambda[K] => e == lifting.liftDepFunExpr[K](other).value(x)
    case _ => false
  }

  override def setType(t: Type): DepLambda[K] = this.copy(x, e)(t)
}

final case class DepApp[K <: Kind](f: Expr, x: K#T)
                                  (override val t: Type = TypePlaceholder) extends Expr {
  override def toString: String = ShowLiftCompactTrack(this)

  override def setType(t: Type): DepApp[K] = this.copy(f, x)(t)
}

final case class Literal(d: semantics.Data) extends Expr {
  override val t: Type = d.dataType

  override def toString: String = s"$d"

  override def setType(t: Type): Literal = sys.error("the type of a Literal should never be changed")
}

abstract class Primitive extends Expr {
  def typeScheme: Type
  override def setType(t: Type): Primitive = sys.error("setType method should be overridden")
}
