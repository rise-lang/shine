package lift.core

import lift.core.types._
import lift.core.DSL.TypeEqual

sealed abstract class Expr {
  val t: Type
  def setType(t: Type): Expr
}

final case class Identifier(name: String)
                           (override val t: Type = TypePlaceholder) extends Expr {
  override def toString: String = name
  override def setType(t: Type): Identifier = this.copy(name)(t)
  override def equals(obj: Any): Boolean = obj match {
    case other: Identifier => (other.name == name) && (other.t =~= t)
    case _ => false
  }
}

final case class Lambda(x: Identifier, e: Expr)
                       (override val t: Type = TypePlaceholder) extends Expr {
  override def toString: String = s"λ$x. $e"
  override def setType(t: Type): Lambda = this.copy(x, e)(t)
  override def equals(obj: Any): Boolean = obj match {
    case other: Lambda => (other.x.t =~= x.t) && (e == lifting.liftFunExpr(other).value(x)) && (other.t =~= t)
    case _ => false
  }
}

final case class App(f: Expr, e: Expr)
                    (override val t: Type = TypePlaceholder) extends Expr {
  override def toString: String = e match {
    case App(App(_,_),_) => s"($f\n$e)"
    case _ => s"($f $e)"
  }
  override def setType(t: Type): App = this.copy(f, e)(t)
  override def equals(obj: Any): Boolean = obj match {
    case other: App => (other.f == f) && (other.e == e) && (other.t =~= t)
    case _ => false
  }
}

final case class DepLambda[K <: Kind](x: K#I, e: Expr)
                                     (override val t: Type = TypePlaceholder)
                                     (implicit val kn: KindName[K]) extends Expr {
  override def toString: String = s"Λ${x.name}: ${kn.get}. $e"
  override def setType(t: Type): DepLambda[K] = this.copy(x, e)(t)
  override def equals(obj: Any): Boolean = obj match {
    case other: DepLambda[K] => (e == lifting.liftDepFunExpr[K](other).value(x)) && (other.t =~= t)
    case _ => false
  }
}

final case class DepApp[K <: Kind](f: Expr, x: K#T)
                                  (override val t: Type = TypePlaceholder) extends Expr {
  override def toString: String = s"($f $x)"
  override def setType(t: Type): DepApp[K] = this.copy(f, x)(t)
  override def equals(obj: Any): Boolean = obj match {
    case other: DepApp[K] => (other.f == f) && (other.x == x) && (other.t =~= t)
    case _ => false
  }
}

final case class Literal(d: semantics.Data) extends Expr {
  override val t: Type = d.dataType
  override def toString: String = s"$d"
  override def setType(t: Type): Literal =
    throw TypeException("tried to set the type of a Literal, whose type should never be changed")
}

abstract class Primitive extends Expr {
  def typeScheme: Type
  override def setType(t: Type): Primitive =
    throw TypeException("setType method should be overridden")
}