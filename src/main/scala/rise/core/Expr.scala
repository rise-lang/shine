package rise.core

import semantics._
import rise.core.types._
import rise.core.ShowRise._
import rise.core.equality._

sealed abstract class Expr {
  val t: Type
  def setType(t: Type): Expr
  override def toString: String = showRise(this)
  def =~=(b : Expr) : Boolean = exprAlphaEq(typeAlphaEq).apply(this)(b)
  def =~~=(b : Expr) : Boolean = exprAlphaEq(typePartialAlphaEq).apply(this)(b)
}

final case class Identifier(name: String)(
    override val t: Type
) extends Expr {
  override def setType(t: Type): Identifier = this.copy(name)(t)
}

final case class Lambda(x: Identifier, e: Expr)(
    override val t: Type
) extends Expr {
  override def setType(t: Type): Lambda = this.copy(x, e)(t)
}

final case class App(f: Expr, e: Expr)(override val t: Type)
    extends Expr {
  override def setType(t: Type): App = this.copy(f, e)(t)
}

final case class DepLambda[K <: Kind: KindName](x: K#I, e: Expr)(override val t: Type) extends Expr {
  val kindName: String = implicitly[KindName[K]].get
  override def setType(t: Type): DepLambda[K] = this.copy(x, e)(t)
}

final case class DepApp[K <: Kind](f: Expr, x: K#T)(
    override val t: Type
) extends Expr {
  override def setType(t: Type): DepApp[K] = this.copy(f, x)(t)
}

final case class Literal(d: semantics.Data) extends Expr {
  override val t: Type = d.dataType
  override def setType(t: Type): Literal =
    if (t != this.t) {
      throw TypeException(s"cannot set the type of ${getClass}")
    } else {
      this
    }
}

final case class Opaque(e: Expr, override val t: Type) extends Expr {
  override def toString: String = s"{Opaque Expr: $t}"
  override def setType(t: Type): TypeAnnotation =
    throw TypeException(s"cannot set the type of ${getClass}")
}
case class TypeAnnotation(e: Expr, annotation: Type) extends Expr {
  override val t : Type = TypePlaceholder
  override def toString: String = s"$e: $annotation"
  override def setType(t: Type): TypeAnnotation =
    if (t != this.t) {
      throw TypeException(s"cannot set the type of ${getClass}")
    } else {
      this
    }
}
case class TypeAssertion(e: Expr, assertion: Type) extends Expr {
  override val t : Type = TypePlaceholder
  override def toString: String = s"$e !: $assertion"
  override def setType(t: Type): TypeAnnotation =
    throw TypeException(s"cannot set the type of ${getClass}")
}

abstract class Primitive extends Expr {
  override val t: Type = TypePlaceholder
  def primEq(obj : Primitive) : Boolean
  def typeScheme: Type =
    throw TypeException(s"typeScheme method must be overridden by ${getClass}")
  def name: String =
    throw RenderException(s"the name of Primitive must be set by ${getClass}")
  override def setType(t: Type): Primitive =
    throw TypeException(s"setType method must be overridden by ${getClass}")
}
