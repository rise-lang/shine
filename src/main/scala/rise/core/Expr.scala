package rise.core

import rise.core.types._
import rise.core.ShowRise._
import rise.core.equality._

sealed abstract class Expr {
  val t: ExprType
  def setType(t: ExprType): Expr
  override def toString: String = showRise(this)
  def =~=(b : Expr) : Boolean = exprAlphaEq(typeAlphaEq).apply(this)(b)
  def =~~=(b : Expr) : Boolean = exprAlphaEq(typePartialAlphaEq).apply(this)(b)
}

final case class Identifier(name: String)(override val t: ExprType) extends Expr {
  override def setType(t: ExprType): Identifier = this.copy(name)(t)
}

final case class Lambda(x: Identifier, e: Expr)(override val t: ExprType) extends Expr {
  override def setType(t: ExprType): Lambda = this.copy(x, e)(t)
}

final case class App(f: Expr, e: Expr)(override val t: ExprType) extends Expr {
  override def setType(t: ExprType): App = this.copy(f, e)(t)
}

final case class DepLambda[T, I](kind: Kind[T, I], x: I, e: Expr)(override val t: ExprType) extends Expr {
  override def setType(t: ExprType): DepLambda[T, I] = this.copy(kind, x, e)(t)
}

final case class DepApp[T](kind: Kind[T, _], f: Expr, x: T)(override val t: ExprType) extends Expr {
  override def setType(t: ExprType): DepApp[T] = this.copy(kind, f, x)(t)
}

final case class Literal(d: semantics.Data) extends Expr {
  override val t: ExprType = d.dataType
  override def setType(t: ExprType): Literal =
    if (t != this.t) { throw TypeException(s"cannot set the type of ${getClass}") } else { this }
}

final case class Opaque(e: Expr, override val t: ExprType) extends Expr {
  override def toString: String = s"{Opaque Expr: $t}"
  override def setType(t: ExprType): TypeAnnotation =
    throw TypeException(s"cannot set the type of ${getClass}")
}

final case class TypeAnnotation(e: Expr, annotation: ExprType) extends Expr {
  override val t : ExprType = TypePlaceholder
  override def toString: String = s"$e: $annotation"
  override def setType(t: ExprType): TypeAnnotation =
    if (t != this.t) { throw TypeException(s"cannot set the type of ${getClass}") } else { this }
}

final case class TypeAssertion(e: Expr, assertion: ExprType) extends Expr {
  override val t : ExprType = TypePlaceholder
  override def toString: String = s"$e !: $assertion"
  override def setType(t: ExprType): TypeAnnotation = throw TypeException(s"cannot set the type of ${getClass}")
}

abstract class Primitive extends Expr {
  override val t: ExprType = TypePlaceholder
  def primEq(obj : Primitive) : Boolean
  def typeScheme: ExprType =
    throw TypeException(s"typeScheme method must be overridden by ${getClass}")
  def name: String =
    throw RenderException(s"the name of Primitive must be set by ${getClass}")
  override def setType(t: ExprType): Primitive =
    throw TypeException(s"setType method must be overridden by ${getClass}")
}
