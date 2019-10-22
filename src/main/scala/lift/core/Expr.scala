package lift.core

import lift.core.types._
import lift.core.DSL.freshTypeIdentifier

sealed abstract class Expr {
  val t: Type
  def setType(t: Type): Expr
}

final case class Identifier(name: String)
                           (override val t: Type = freshTypeIdentifier) extends Expr {
  override def toString: String = name
  override def setType(t: Type): Identifier = this.copy(name)(t)
}

final case class Lambda(x: Identifier, e: Expr)
                       (override val t: Type = freshTypeIdentifier) extends Expr {
  override def toString: String = s"λ$x. $e"

  override def equals(obj: Any): Boolean = obj match {
    case other: Lambda => e == lifting.liftFunExpr(other).value(x)
    case _ => false
  }

  override def setType(t: Type): Lambda = this.copy(x, e)(t)
}

final case class Apply(f: Expr, e: Expr)
                      (override val t: Type = freshTypeIdentifier) extends Expr {
  override def toString: String = e match {
    case Apply(Apply(_,_),_) => s"($f\n$e)"
    case _ => s"($f $e)"
  }

  override def setType(t: Type): Apply = this.copy(f, e)(t)
}

final case class DepLambda[K <: Kind](x: K#I, e: Expr)
                                     (override val t: Type = freshTypeIdentifier)
                                     (implicit val kn: KindName[K]) extends Expr {
  override def toString: String = s"Λ${x.name}: ${kn.get}. $e"

  override def equals(obj: Any): Boolean = obj match {
    case other: DepLambda[K] => e == lifting.liftDepFunExpr[K](other).value(x)
    case _ => false
  }

  override def setType(t: Type): DepLambda[K] = this.copy(x, e)(t)
}

final case class DepApply[K <: Kind](f: Expr, x: K#T)
                                    (override val t: Type = freshTypeIdentifier) extends Expr {
  override def toString: String = s"($f $x)"

  override def setType(t: Type): DepApply[K] = this.copy(f, x)(t)
}

final case class Literal(d: semantics.Data) extends Expr {
  override val t: Type = d.dataType

  override def toString: String = s"$d"

  override def setType(t: Type): Literal = this.copy(d)
}

/*
final case class TypedExpr(e: Expr, t: Type) extends Expr {
  override def toString: String = s"($e: $t)"
}
*/

abstract class Primitive extends Expr {
  def typeScheme: Type
  override def setType(t: Type): Primitive = this
}
