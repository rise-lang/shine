package rise.core.types

import arithexpr.arithmetic.RangeAdd
import rise.core._
import rise.core.equality._

sealed trait Type {
  def =~=(b: Type): Boolean = typeAlphaEq[Type](this)(b)
  def =~~=(b: Type): Boolean = typePartialAlphaEq[Type](this)(b)
}

object TypePlaceholder extends Type {
  override def toString: String = "?"
}

final case class TypeIdentifier(name: String) extends Type {
  override def toString: String = "_" + name
}

final case class FunType[T <: Type, U <: Type](inT: T, outT: U) extends Type {
  override def toString: String = s"($inT -> $outT)"
}

final case class DepFunType[T, I, U <: Type] (kind: Kind[T, I],x: I, t: U) extends Type {
  override def toString: String = s"(${Kind.idName(kind, x)}: ${kind.name} -> $t)"
}

// == Data types ==============================================================

sealed trait DataType extends Type

object DataType {
  final case class DataTypeIdentifier(name: String) extends DataType {
    override def toString: String = name
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

  final case class OpaqueType(name: String) extends DataType {
    override def toString: String = name
  }

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

  final case class FragmentType(rows: Nat,
                                columns: Nat,
                                d3: Nat,
                                dataType: DataType,
                                fragmentKind: Fragment,
                                layout: MatrixLayout) extends DataType {
    override def toString: String =
      if (fragmentKind == Fragment.Accumulator)
        s"Fragment[$rows,$columns,$d3,$dataType,$fragmentKind]"
      else
        s"Fragment[$rows,$columns,$d3,$dataType,$fragmentKind,$layout]"

    override def equals(o: Any): Boolean = o match {
      case f: FragmentType =>
        f.fragmentKind match {
          case Fragment.Accumulator =>
            f.rows.equals(rows) && f.columns.equals(columns) && f.d3.equals(d3) && f.dataType.equals(dataType)
          case _ =>
            f.rows.equals(rows) && f.columns.equals(columns) && f.d3.equals(d3) && f.dataType.equals(dataType) &&
              f.fragmentKind.equals(fragmentKind) && f.layout.equals(layout)
        }
      case _ => false
    }
  }

  final case class ManagedBufferType(dt: DataType) extends DataType {
    override def toString: String = s"managed[$dt]"
  }

  final case class DepPairType[T, I](kind: Kind[T, I], x: I, t: DataType) extends DataType {
    override def toString: String = s"(${Kind.idName(kind, x)}: ${kind.name} ** $t)"

    override def equals(other: Any): Boolean =other match {
      case DepPairType(k2, x2, elemT2) =>
        kind == k2 && (kind match {
          case _: NatKind.type =>
            this.t == substitute.natInType(this.x.asInstanceOf[NatIdentifier], x2.asInstanceOf[NatIdentifier], elemT2)
          case _ => ???
        })
      case _ => false
    }
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
}
