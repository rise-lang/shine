package rise.core

import semantics._
import rise.core.types._
import rise.core.DSL.Type.TypeEqual
import rise.core.ShowRise._

object alphaEquiv {
  val equiv : Expr => Expr => Boolean = a => b => {
    a.t =~= b.t && ((a, b) match {
      case (Identifier(na), Identifier(nb)) => na == nb
      case (Literal(da), Literal(db)) => da == db
      case (a : Primitive, b : Primitive) => a.name == b.name
      // Application compares structurally
      case (App(fa, ea), App(fb, eb)) => equiv(fa)(fb) && equiv(ea)(eb)
      case (DepApp(fa, xa), DepApp(fb, xb)) => equiv(fa)(fb) && xa == xb
      // Abstraction compares after substitution
      case (Lambda(xa, ta), other@Lambda(xb, _)) =>
        xa.t =~= xb.t && equiv(ta)(typedLifting.liftFunExpr(other).value(xa))
      case (DepLambda(xa, ea), other@DepLambda(xb, _)) => (xa, xb) match {
        case (n: NatIdentifier, _: NatIdentifier) =>
          equiv(ea)(typedLifting.liftDepFunExpr[NatKind](other).value(n))
        case (dt: DataTypeIdentifier, _: DataTypeIdentifier) =>
          equiv(ea)(typedLifting.liftDepFunExpr[DataKind](other).value(dt))
        case (addr: AddressSpaceIdentifier, _: AddressSpaceIdentifier) =>
          equiv(ea)(typedLifting.liftDepFunExpr[AddressSpaceKind](other).value(addr))
        case (n2n: NatToNatIdentifier, _: NatToNatIdentifier) =>
          equiv(ea)(typedLifting.liftDepFunExpr[NatToNatKind](other).value(n2n))
        case (n2d: NatToDataIdentifier, _: NatToDataIdentifier) =>
          equiv(ea)(typedLifting.liftDepFunExpr[NatToDataKind](other).value(n2d))
        case _ => false
      }
      case _ => false
    })
  }

  val hash : Expr => Int = {
    case _: Identifier => 17
    case Lambda(_, e) => 3 * e.hashCode() + 1
    case App(f, e) => 5 * f.hashCode() + -7 * e.hashCode() + 2
    case DepLambda(_, e) => 4 * e.hashCode() + 3
    case DepApp(f, _) => 6 * f.hashCode() + 4
    case l@Literal(_: ScalarData | _: VectorData) => l.d.hashCode()
    case Literal(_: NatData) => 91
    case Literal(_: IndexData) => 93
    case Literal(_: ArrayData) => 95
    case Literal(_: PairData) => 97
    case p: Primitive => p.getClass.hashCode()
  }
}

sealed abstract class Expr {
  val t: Type
  def setType(t: Type): Expr
  override def toString: String = showRise(this)
  override def hashCode(): Int = alphaEquiv.hash(this)
  override def equals(obj : Any) : Boolean = obj match {
    case other : Expr => alphaEquiv.equiv(this)(other)
    case _ => true
  }
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

final case class DepLambda[K <: Kind: KindName](
    x: K#I with Kind.Explicitness,
    e: Expr
)(override val t: Type)
    extends Expr {
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
    throw TypeException(
      "tried to set the type of a Literal, whose type should never be changed"
    )
}

abstract class Primitive extends Expr {
  override val t: Type = TypePlaceholder
  def typeScheme: Type =
    throw TypeException("typeScheme method must be overridden")
  def name: String =
    throw RenderException("the name of Primitive must be set")
  override def setType(t: Type): Primitive =
    throw TypeException("setType method must be overridden")
}
