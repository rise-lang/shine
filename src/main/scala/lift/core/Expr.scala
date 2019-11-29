package lift.core

import lift.core.types._
import lift.core.DSL.TypeEqual
import lift.core.ShowLift._

sealed abstract class Expr {
  val t: Type
  def setType(t: Type): Expr
  override def toString: String = showLift(this)
}

final case class Identifier(name: String)
                           (override val t: Type = TypePlaceholder) extends Expr {
  override def setType(t: Type): Identifier = this.copy(name)(t)
  override def equals(obj: Any): Boolean = obj match {
    case other: Identifier => (other.name == name) && (other.t =~= t)
    case _ => false
  }
}

final case class Lambda(x: Identifier, e: Expr)
                       (override val t: Type = TypePlaceholder) extends Expr {
  override def setType(t: Type): Lambda = this.copy(x, e)(t)
  override def equals(obj: Any): Boolean = obj match {
    case other: Lambda => (other.x.t =~= x.t) && (e == lifting.liftFunExpr(other).value(x)) && (other.t =~= t)
    case _ => false
  }
}

final case class App(f: Expr, e: Expr)
                    (override val t: Type = TypePlaceholder) extends Expr {
  override def setType(t: Type): App = this.copy(f, e)(t)
  override def equals(obj: Any): Boolean = obj match {
    case other: App => (other.f == f) && (other.e == e) && (other.t =~= t)
    case _ => false
  }
}

final case class DepLambda[K <: Kind](x: K#I, e: Expr)
                                     (override val t: Type = TypePlaceholder)
                                     (implicit val kn: KindName[K]) extends Expr {
  override def setType(t: Type): DepLambda[K] = this.copy(x, e)(t)
  override def equals(obj: Any): Boolean = obj match {
    case other: DepLambda[K] => (e == lifting.liftDepFunExpr[K](other).value(x)) && (other.t =~= t)
    case _ => false
  }
}

final case class DepApp[K <: Kind](f: Expr, x: K#T)
                                  (override val t: Type = TypePlaceholder) extends Expr {
  override def setType(t: Type): DepApp[K] = this.copy(f, x)(t)
  override def equals(obj: Any): Boolean = obj match {
    case other: DepApp[K] => (other.f == f) && (other.x == x) && (other.t =~= t)
    case _ => false
  }
}

final case class Literal(d: semantics.Data) extends Expr {
  override val t: Type = d.dataType
  override def setType(t: Type): Literal =
    throw TypeException("tried to set the type of a Literal, whose type should never be changed")
}

abstract class Primitive extends Expr {
  def typeScheme: Type
  val name: String = "Unknown Primitive"
  override def setType(t: Type): Primitive =
    throw TypeException("setType method should be overridden")
}