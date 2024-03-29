package rise.eqsat

import rise.core.{types => rct}
import rise.core.types.{DataType => rcdt}

/** A Rise type based on DeBruijn indexing */
case class Type(node: TypeNode[Type, Nat, DataType]) {
  override def toString: String = node.toString
}

case class DataType(node: DataTypeNode[Nat, DataType]) {
  override def toString: String = node.toString
}

sealed trait TypePattern {
  def patternVars(): Set[Any] = {
    this match {
      case TypePatternNode(n) =>
        TypeNode.collect(n.map(_.patternVars(), _.patternVars(), _.patternVars())).flatten.toSet
      case pv: TypePatternVar => Set(pv)
      case TypePatternAny => Set()
      case dt: DataTypePattern => dt.patternVars()
    }
  }
}
case class TypePatternNode(n: TypeNode[TypePattern, NatPattern, DataTypePattern])
  extends TypePattern {
  override def toString: String = n.toString
}
case class TypePatternVar(index: Int) extends TypePattern {
  override def toString: String = s"?t$index"
}
case object TypePatternAny extends TypePattern {
  override def toString: String = "?t"
}

sealed trait DataTypePattern extends TypePattern {
  override def patternVars(): Set[Any] = {
    this match {
      case DataTypePatternNode(n) =>
        DataTypeNode.collect(n.map(_.patternVars(), _.patternVars())).flatten.toSet
      case pv: DataTypePatternVar => Set(pv)
      case DataTypePatternAny => Set()
    }
  }
}
case class DataTypePatternNode(n: DataTypeNode[NatPattern, DataTypePattern])
  extends DataTypePattern {
  override def toString: String = n.toString
}
case class DataTypePatternVar(index: Int) extends DataTypePattern {
  override def toString: String = s"?dt$index"
}
case object DataTypePatternAny extends DataTypePattern {
  override def toString: String = "?dt"
}

object Type {
  /** Shift nat and datatype indices */
  type Shift = (Int, Int)

  def fromNamed(t: rct.ExprType, bound: Expr.Bound = Expr.Bound.empty): Type = {
    Type(t match {
      case dt: rct.DataType => DataType.fromNamed(dt, bound).node
      case rct.FunType(a, b) => FunType(fromNamed(a, bound), fromNamed(b, bound))
      case rct.DepFunType(rct.NatKind, x: rct.NatIdentifier, t) => NatFunType(fromNamed(t, bound + x))
      case rct.DepFunType(rct.DataKind, x: rcdt.DataTypeIdentifier, t) => DataFunType(fromNamed(t, bound + x))
      case rct.DepFunType(rct.AddressSpaceKind, x: rct.AddressSpaceIdentifier, t) => AddrFunType(fromNamed(t, bound + x))
      case rct.DepFunType(_, _, _) => ???
      case rct.TypePlaceholder | rct.TypeIdentifier(_) =>
        throw new Exception(s"did not expect $t")
    })
  }

  def toNamed(t: Type, bound: Expr.Bound = Expr.Bound.empty): rct.ExprType = {
    t.node match {
      case dt: DataTypeNode[Nat, DataType] => DataType.toNamed(DataType(dt), bound)
      case FunType(a, b) => rct.FunType(toNamed(a, bound), toNamed(b, bound))
      case NatFunType(t) =>
        val i = rct.NatIdentifier(s"n${bound.nat.size}")
        rct.DepFunType(rct.NatKind, i, toNamed(t, bound + i))
      case DataFunType(t) =>
        val i = rcdt.DataTypeIdentifier(s"n${bound.data.size}")
        rct.DepFunType(rct.DataKind, i, toNamed(t, bound + i))
      case AddrFunType(t) =>
        val i = rct.AddressSpaceIdentifier(s"a${bound.data.size}")
        rct.DepFunType(rct.AddressSpaceKind, i, toNamed(t, bound + i))
    }
  }

  def simplifyNats(t: Type): Type =
    Type(t.node.map(simplifyNats, Nat.simplify, DataType.simplifyNats))
}

object DataType {
  def fromNamed(dt: rct.DataType, bound: Expr.Bound = Expr.Bound.empty): DataType = {
    DataType(dt match {
      case i: rcdt.DataTypeIdentifier => DataTypeVar(bound.indexOf(i))
      case s: rcdt.ScalarType => ScalarType(s)
      case rcdt.NatType => NatType
      case rcdt.VectorType(s, et) => VectorType(Nat.fromNamed(s, bound), fromNamed(et, bound))
      case rcdt.IndexType(s) => IndexType(Nat.fromNamed(s, bound))
      case rcdt.PairType(dt1, dt2) => PairType(fromNamed(dt1, bound), fromNamed(dt2, bound))
      case rcdt.ArrayType(s, et) => ArrayType(Nat.fromNamed(s, bound), fromNamed(et, bound))
      case _: rcdt.DepArrayType | _: rcdt.DepPairType[_, _] |
           _: rcdt.NatToDataApply | _: rcdt.FragmentType | _: rcdt.ManagedBufferType | _: rcdt.OpaqueType =>
        throw new Exception(s"did not expect $dt")
    })
  }

  def toNamed(dt: DataType, bound: Expr.Bound = Expr.Bound.empty): rct.DataType = {
    dt.node match {
      case DataTypeVar(index) => bound.getData(index)
      case ScalarType(s) => s
      case NatType => rcdt.NatType
      case VectorType(s, et) => rcdt.VectorType(Nat.toNamed(s, bound), toNamed(et, bound))
      case IndexType(s) => rcdt.IndexType(Nat.toNamed(s, bound))
      case PairType(dt1, dt2) => rcdt.PairType(toNamed(dt1, bound), toNamed(dt2, bound))
      case ArrayType(s, et) => rcdt.ArrayType(Nat.toNamed(s, bound), toNamed(et, bound))
    }
  }

  def simplifyNats(dt: DataType): DataType =
    DataType(dt.node.map(Nat.simplify, simplifyNats))
}

object TypePattern {
  def fromType(t: Type): TypePattern = {
    val pnode = t.node.map(fromType, NatPattern.fromNat, DataTypePattern.fromDataType)
    TypePatternNode(pnode)
  }
}

object DataTypePattern {
  def fromDataType(dt: DataType): DataTypePattern = {
    val pnode = dt.node.map(NatPattern.fromNat, fromDataType)
    DataTypePatternNode(pnode)
  }
}

sealed trait TypeNode[+T, +N, +DT] {
  def map[TO, NO, DTO](ft: T => TO,
                       fn: N => NO,
                       fdt: DT => DTO): TypeNode[TO, NO, DTO] =
    this match {
      case FunType(a, b) => FunType(ft(a), ft(b))
      case NatFunType(t) => NatFunType(ft(t))
      case DataFunType(t) => DataFunType(ft(t))
      case AddrFunType(t) => AddrFunType(ft(t))
      case dt: DataTypeNode[N, DT] => dt.map(fn, fdt)
    }

  final def childrenCount(): Int = {
    var c = 0
    map(_ => c += 1, _ => c += 1, _ => c += 1)
    c
  }
}
// final case class TypeVar(index: Int) extends TypeNode[Nothing]
final case class FunType[T](inT: T, outT: T) extends TypeNode[T, Nothing, Nothing] {
  override def toString: String = s"($inT -> $outT)"
}
final case class NatFunType[T](t: T) extends TypeNode[T, Nothing, Nothing] {
  override def toString: String = s"(nat) -> $t"
}
final case class DataFunType[T](t: T) extends TypeNode[T, Nothing, Nothing] {
  override def toString: String = s"(data) -> $t"
}
final case class AddrFunType[T](t: T) extends TypeNode[T, Nothing, Nothing] {
  override def toString: String = s"(addr) -> $t"
}

sealed trait DataTypeNode[+N, +DT] extends TypeNode[Nothing, N, DT] {
  def map[NO, DTO](fn: N => NO,
                   fdt: DT => DTO): DataTypeNode[NO, DTO] =
    this match {
      case DataTypeVar(index) => DataTypeVar(index)
      case ScalarType(s) => ScalarType(s)
      case NatType => NatType
      case VectorType(s, dt) => VectorType(fn(s), fdt(dt))
      case IndexType(s) => IndexType(fn(s))
      case PairType(a, b) => PairType(fdt(a), fdt(b))
      case ArrayType(s, dt) => ArrayType(fn(s), fdt(dt))
    }
}
final case class DataTypeVar(index: Int) extends DataTypeNode[Nothing, Nothing] {
  override def toString: String = s"%dt$index"
}
final case class ScalarType(s: rcdt.ScalarType) extends DataTypeNode[Nothing, Nothing] {
  override def toString: String = s.toString
}
case object NatType extends DataTypeNode[Nothing, Nothing] {
  override def toString: String = "nat"
}
final case class VectorType[N, DT](size: N, elemType: DT) extends DataTypeNode[N, DT] {
  override def toString: String = s"<$size>$elemType"
}
final case class IndexType[N](size: N) extends DataTypeNode[N, Nothing] {
  override def toString: String = s"idx[$size]"
}
final case class PairType[DT](dt1: DT, dt2: DT) extends DataTypeNode[Nothing, DT] {
  override def toString: String = s"($dt1 x $dt2)"
}
final case class ArrayType[N, DT](size: N, elemType: DT) extends DataTypeNode[N, DT] {
  override def toString: String = s"$size.$elemType"
}

object TypeNode {
  def collect[T](n: TypeNode[T, T, T]): Seq[T] = n match {
    case FunType(inT, outT) => Seq(inT, outT)
    case NatFunType(t) => Seq(t)
    case DataFunType(t) => Seq(t)
    case AddrFunType(t) => Seq(t)
    case dt: DataTypeNode[T, T] => DataTypeNode.collect(dt)
  }
}

object DataTypeNode {
  def collect[T](n: DataTypeNode[T, T]): Seq[T] = n match {
    case DataTypeVar(_) => Seq()
    case ScalarType(_) => Seq()
    case NatType => Seq()
    case VectorType(size, elemType) => Seq(size, elemType)
    case IndexType(size) => Seq(size)
    case PairType(dt1, dt2) => Seq(dt1, dt2)
    case ArrayType(size, elemType) => Seq(size, elemType)
  }
}
