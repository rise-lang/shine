package rise.core.types

import arithexpr.arithmetic.RangeAdd
import rise.core._

sealed trait Type

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

  override def equals(obj: Any): Boolean = obj match {
    case other: DepFunType[K, _] =>
      t == lifting.liftDependentFunctionType[K](other)(x)
    case _ => false
  }
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
  override def equals(that: Any): Boolean = that match {
    case d: DataTypeIdentifier => this.name == d.name
    case _                     => false
  }
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

sealed trait MatrixLayout

object MatrixLayout {
  object Row_Major extends MatrixLayout { override def toString = "Row_Major" }
  object Col_Major extends MatrixLayout { override def toString = "Col_Major" }
}

final case class MatrixLayoutIdentifier(
                                         name: String,
                                         override val isExplicit: Boolean = false
                                       ) extends MatrixLayout
  with Kind.Identifier
  with Kind.Explicitness {
  override def toString: String = if (isExplicit) name else "_" + name
  override def asExplicit: MatrixLayoutIdentifier = this.copy(isExplicit = true)
  override def asImplicit: MatrixLayoutIdentifier =
    this.copy(isExplicit = false)
  override def equals(that: Any): Boolean = that match {
    case a: MatrixLayoutIdentifier => this.name == a.name
    case _                         => false
  }
  override def hashCode(): Int = this.name.hashCode()
}

sealed trait WmmaFragment extends DataType {
  def m:Nat
  def n:Nat
  def k:Nat
  def dataType: DataType

  def arrayType: ArrayType
}

final case class WmmaAMatrix(m: Nat,
                             n: Nat,
                             k: Nat,
                             dataType: DataType,
                             layout: MatrixLayout
                            ) extends WmmaFragment {
  override def arrayType: ArrayType = ArrayType(m, ArrayType(k, dataType))

  override def toString: String = s"wmmaAMatrix[$m,$n,$k,$dataType $layout]"
}

final case class WmmaBMatrix(m: Nat,
                             n: Nat,
                             k: Nat,
                             dataType: DataType,
                             layout: MatrixLayout
                            ) extends WmmaFragment {
  override def arrayType: ArrayType = ArrayType(k, ArrayType(n, dataType))

  override def toString: String = s"wmmaBMatrix[$m,$n,$k,$dataType $layout]"
}

final case class WmmaAccumulator(m: Nat,
                                 n: Nat,
                                 k: Nat,
                                 dataType: DataType
                                ) extends WmmaFragment {
  override def arrayType: ArrayType = ArrayType(m, ArrayType(n, dataType))

  override def toString: String = s"WmmaAccumulator[$m,$n,$k,$dataType]"
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

  override def equals(obj: Any): Boolean = obj match {
    case other: DepPairType[K] =>
      t == substitute.kindInType[K, DataType](
        this.x, `for` = other.x, in = other.t
      )
    case _ => false
  }

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
    val n = NatIdentifier(freshName("n"), RangeAdd(0, size, 1))
    DepArrayType(size, NatToDataLambda(n, f(n)))
  }
}
