package rise.core.types

import arithexpr.arithmetic.RangeAdd
import rise.core._

object alphaEquiv {
  val equiv : Type => Type => Boolean = a => b => {
    (a.getClass == b.getClass && b.getClass == TypePlaceholder.getClass) ||
    (a.getClass == b.getClass && b.getClass == NatType.getClass) ||
      ((a, b) match {
      case (TypeIdentifier(na), TypeIdentifier(nb)) => na == nb
      case (DataTypeIdentifier(na, _), DataTypeIdentifier(nb, _)) => na == nb
      case (FunType(sa, ta), FunType(sb, tb)) => equiv(sa)(sb) && equiv(ta)(tb)
      case (DepFunType(xa, ta), other@DepFunType(_, _)) =>
        equiv(ta)(lifting.liftDependentFunctionType(other)(xa))
      case (sa : ScalarType, sb : ScalarType) => sa.getClass == sb.getClass
      case (VectorType(sa, da), VectorType(sb, db)) => sa == sb && equiv(da)(db)
      case (IndexType(sa), IndexType(sb)) => sa == sb
      case (DepPairType(xa, ta), other@DepPairType(xb, tb)) =>
        equiv(ta)(substitute.kindInType(xa, `for` = xb, in = tb))
      case (PairType(la, ra), PairType(lb, rb)) => equiv(la)(lb) && equiv(ra)(rb)
      case (NatToDataApply(fa, na), NatToDataApply(fb, nb)) => fa == fb && na == nb
      case (ArrayType(sa, da), ArrayType(sb, db)) => sa == sb && equiv(da)(db)
      case (DepArrayType(sa, da), DepArrayType(sb, db)) => sa == sb && da == db
      case _ => false
    })
  }

  val hash : Type => Int = {
    case TypePlaceholder => 5
    case TypeIdentifier(n) => 7 * n.hashCode()
    case FunType(inT, outT) => 11 * inT.hashCode() + 13 * outT.hashCode() + 1
    case DepFunType(_, t) => 17 * t.hashCode()
    case dataType: DataType => 19 * dataType.hashCode()
  }
}


sealed trait Type {
  override def hashCode(): Int = alphaEquiv.hash(this)

  override def equals(o: Any): Boolean = o match {
    case other : Type => alphaEquiv.equiv(this)(other)
    case _ => false
  }
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
  override def hashCode(): Int = this.name.hashCode()
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

  override def hashCode(): Int = super.hashCode()
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
