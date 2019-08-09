package lift.core

import lift.core.types._

sealed trait Expr

final case class Identifier(name: String) extends Expr {
  override def toString: String = name
}

final case class Lambda(x: Identifier, e: Expr) extends Expr {
  override def toString: String = s"λ$x. $e"

  override def equals(obj: Any): Boolean = obj match {
    case other: Lambda => e == lifting.liftFunExpr(other).value(x)
    case _ => false
  }
}

final case class Apply(f: Expr, e: Expr) extends Expr {
  override def toString: String = e match {
    case Apply(Apply(_,_),_) => s"($f\n$e)"
    case _ => s"($f $e)"
  }
}

final case class DepLambda[K <: Kind](x: K#I, e: Expr)
                                     (implicit val kn: KindName[K]) extends Expr {
  override def toString: String = s"Λ${x.name}: ${kn.get}. $e"

  override def equals(obj: Any): Boolean = obj match {
    case other: DepLambda[K] => e == lifting.liftDepFunExpr[K](other).value(x)
    case _ => false
  }
}

final case class DepApply[K <: Kind](f: Expr, x: K#T) extends Expr {
  override def toString: String = s"($f $x)"
}

final case class Literal(d: semantics.Data) extends Expr {
  override def toString: String = s"$d"
}

final case class TypedExpr(e: Expr, t: Type) extends Expr {
  override def toString: String = s"($e: $t)"
}

abstract class Primitive extends Expr {
  def t: Type
}