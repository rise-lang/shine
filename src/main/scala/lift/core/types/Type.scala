package lift.core.types

import lift.core._
import lift.arithmetic._
import lift.core.DSL.n2dtFun

sealed trait Type

final case class TypeIdentifier(name: String) extends Type {
  override def toString: String = name
}

final case class FunType[T1 <: Type, T2 <: Type](inT: T1, outT: T2) extends Type {
  override def toString: String = s"($inT -> $outT)"
}

final case class DepFunType[K <: Kind, T <: Type](x: K#I, t: T) extends Type {
  override def toString: String =
    s"(${x.name}: ${Kind.formatKindName(x.getClass.getName)} -> $t)"

  override def equals(obj: Any) = obj match {
    case other: DepFunType[K, _] => t == lifting.liftDependentFunctionType[K](other)(x)
    case _ => false
  }
}

final case class NatDataTypeDependentFunctionType[T <: Type](fn: NatToDataIdentifier, t: T) extends Type {
  override def toString: String = s"(${fn.name}:nat -> data -> $t)"
}

case class DataAccessType(dt: DataType, w: AccessType) extends Type {
  override def toString: String = s"${dt}_$w"
}

// ============================================================================================= //
// Nat -> Nat
// ============================================================================================= //
sealed trait NatToNat {
  def apply(n: Nat): Nat = NatToNatApply(this, n)
}

final case class NatToNatLambda private(x: NatIdentifier, body: Nat) extends NatToNat {
  // NatToNat have an interesting comparison behavior, as we do not define
  // equality for them as simple syntactic equality: we just want to make sure their bodies
  // are equal up-to renaming of the binder.

  // However, just updating equals is not sufficient, as many data structures, such as HashMaps,
  // use hashCodes as proxy for equality. In order to make sure this property is respected, we ignore
  // the identifier variable, and just take the hash of the body evaluated at a known point
  override def hashCode(): Int = this(NamedVar("comparisonDummy")).hashCode()

  override def apply(l: Nat): Nat = ArithExpr.substitute(body, Map((x, l)))

  override def toString: String = s"($x: nat |-> $body)"

  override def equals(obj: Any): Boolean = obj match {
    case other: NatToNatLambda => body == other(x)
    case _ => false
  }
}

final case class NatToNatIdentifier(name: String) extends NatToNat with Kind.Identifier {
  override lazy val toString: String = name
}

final class NatToNatApply(val f: NatToNat, val n: Nat) extends ArithExprFunction(s"$f($n)") {
  override def visitAndRebuild(f: Nat => Nat): Nat = this
  override lazy val toString: String = s"$f($n)"
}
object NatToNatApply {
  def apply(f: NatToNat, n: Nat): Nat = f match {
    case l: NatToNatLambda      => l.apply(n)
    case i: NatToNatIdentifier  => new NatToNatApply(i, n)
  }
  def unapply(arg: NatToNatApply): Option[(NatToNat, Nat)] = Some((arg.f, arg.n))
}


// ============================================================================================= //
// Nat -> DataType
// ============================================================================================= //
sealed trait NatToData {
  def map(f:DataType => DataType): NatToData = this match {
    case ident: NatToDataIdentifier => ident
    case NatToDataLambda(x, body) => NatToDataLambda(x, f(body))
  }

  def apply(n: Nat): DataType = NatToDataApply(this, n)
}

case class NatToDataLambda private (x: NatIdentifier, body: DataType) extends NatToData {
  //See hash code of NatNatTypeFunction
  override def hashCode(): Int = this.apply(NamedVar("ComparisonDummy")).hashCode()

  override def apply(a: Nat): DataType = substitute(a, `for`=x, in=body)

  override def toString: String = s"($x: nat |-> $body)"

  override def equals(obj: Any): Boolean = obj match {
    case other:NatToDataLambda => body == other.apply(x)
    case _ => false
  }
}

final case class NatToDataIdentifier(name: String) extends NatToData with Kind.Identifier {
  override def toString: String = name
}