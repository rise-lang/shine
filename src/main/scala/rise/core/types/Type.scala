package rise.core.types

import arithexpr.arithmetic.RangeAdd
import rise.core._
import rise.core.equality._

sealed trait Type {
  def =~~=(b: Type): Boolean = typeAlphaEq[TypeKind](this)(b)
  def =~=(b: Type): Boolean = typeUnificationAlphaEq[TypeKind](this)(b)
}

object TypePlaceholder extends Type {
  override def toString: String = "?"
}

final case class TypeIdentifier(name: String)
    extends Type
    with Kind.Identifier {
  override def toString: String = "_" + name
}

final case class FunType[T <: Type, U <: Type](inT: T, outT: U)
    extends Type {
  override def toString: String = s"($inT -> $outT)"
}

final case class DepFunType[K <: Kind: KindName, T <: Type](
    x: K#I with Kind.Explicitness,
    t: T
) extends Type {
  override def toString: String =
    s"(${x.name}: ${implicitly[KindName[K]].get} -> $t)"
}

// == Data types ==============================================================

sealed trait DataType extends Type

final case class DataTypeIdentifier(name: String,
                                    override val isExplicit: Boolean = false
                                   ) extends DataType
  with Kind.Identifier
  with Kind.Explicitness {
  override def toString: String = if (isExplicit) name else "_" + name
  override def asExplicit: DataTypeIdentifier = this.copy(isExplicit = true)
  override def asImplicit: DataTypeIdentifier = this.copy(isExplicit = false)
}

sealed trait ScalarType extends DataType

object bool extends ScalarType { override def toString: String = "bool" }

object int extends ScalarType { override def toString: String = "int" }

object i8  extends ScalarType { override def toString: String = "i8"  }
object i16 extends ScalarType { override def toString: String = "i16" }
object i32 extends ScalarType { override def toString: String = "i32" }
object i64 extends ScalarType { override def toString: String = "i64" }

object u8  extends ScalarType { override def toString: String = "u8"  }
object u16 extends ScalarType { override def toString: String = "u16" }
object u32 extends ScalarType { override def toString: String = "u32" }
object u64 extends ScalarType { override def toString: String = "u64" }

object f16 extends ScalarType { override def toString: String = "f16" }
object f32 extends ScalarType { override def toString: String = "f32" }
object f64 extends ScalarType { override def toString: String = "f64" }

object NatType extends DataType { override def toString: String = "nat" }

// TODO: enforce ScalarType
sealed case class VectorType(size: Nat, elemType: DataType) extends DataType {
  override def toString: String = s"<$size>$elemType"
}

object vec {
  def apply(size: Nat, elemType: DataType): VectorType =
    VectorType(size, elemType)
}

final case class IndexType(size: Nat) extends DataType {
  override def toString: String = s"idx[$size]"
}

final case class PairType(dt1: DataType, dt2: DataType) extends DataType {
  override def toString: String = s"($dt1, $dt2)"
}


final case class DepPairType[K <: Kind: KindName](
                            x: K#I,
                            t: DataType
                           ) extends DataType {
  type Kind = K

  // Note(federico): for pattern-matching purposes, if we ever need to
  // recover the kind name from a pattern-match over just DataType
  val kindName: KindName[K] = implicitly[KindName[K]]

  override def toString: String =
    s"(${x.name}: ${kindName.get} ** $t)"
}


final class NatToDataApply(val f: NatToData, val n: Nat) extends DataType {
  override def toString: String = s"$f($n)"
}

object NatToDataApply {
  def apply(f: NatToData, n: Nat): DataType = f match {
    case l: NatToDataLambda     => l.apply(n)
    case i: NatToDataIdentifier => new NatToDataApply(i, n)
  }

  def unapply(arg: NatToDataApply): Option[(NatToData, Nat)] =
    Some((arg.f, arg.n))
}

final case class ArrayType(size: Nat, elemType: DataType) extends DataType {
  override def toString: String = s"$size.$elemType"
}

final case class DepArrayType(size: Nat, fdt: NatToData) extends DataType {
  override def toString: String = s"$size..$fdt"
}

object DepArrayType {
  def apply(size: Nat, f: Nat => DataType): DepArrayType = {
    val n = NatIdentifier(freshName("n"), RangeAdd(0, size, 1), isExplicit = true)
    DepArrayType(size, NatToDataLambda(n, f(n)))
  }
}
