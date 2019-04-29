package lift.core

import lift.core.types._

sealed trait Expr

final case class Identifier(name: String) extends Expr {
  override def toString: String = name
}

final case class Lambda(x: Identifier, e: Expr) extends Expr {
  override def toString: String = s"λ$x. $e"
}

final case class Apply(f: Expr, e: Expr) extends Expr {
  override def toString: String = s"($f $e)"
}

final case class NatLambda(n: NatIdentifier, e: Expr) extends Expr {
  override def toString: String = s"Λ($n : nat). $e"
}

final case class NatApply(f: Expr, n: Nat) extends Expr {
  override def toString: String = s"($f $n)"
}

final case class TypeLambda(dt: DataTypeIdentifier, e: Expr) extends Expr {
  override def toString: String = s"Λ($dt : data). $e"
}

final case class TypeApply(f: Expr, dt: DataType) extends Expr {
  override def toString: String = s"($f $dt)"
}

final case class Literal(d: semantics.Data) extends Expr {
  override def toString: String = s"$d"
}

final case class Index(n: Nat, size: Nat) extends Expr {
  override def toString: String = s"idx($n)"
}

final case class NatExpr(n: Nat) extends Expr {
  override def toString: String = s"$n"
}

final case class TypedExpr(e: Expr, t: Type) extends Expr {
  override def toString: String = s"($e : $t)"
}

abstract class Primitive extends Expr {
  def t: Type
}