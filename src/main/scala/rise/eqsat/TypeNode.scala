package rise.eqsat

import rise.core.{types => rct}

case class Type(node: TypeNode[Type, Nat, DataType]) {
  override def toString: String = node.toString
}
case class DataType(node: DataTypeNode[Nat, DataType]) {
  override def toString: String = node.toString
}

sealed trait TypePattern
case class TypePatternNode(n: TypeNode[TypePattern, NatPattern, DataTypePattern])
  extends TypePattern
case class TypePatternVar(index: Int) extends TypePattern

sealed trait DataTypePattern extends TypePattern
case class DataTypePatternNode(n: DataTypeNode[NatPattern, DataTypePattern])
  extends DataTypePattern
case class DataTypePatternVar(index: Int) extends DataTypePattern

object Type {
  def fromNamed(t: rct.Type, bound: Expr.Bound = Expr.Bound.empty): Type = {
    Type(t match {
      case dt: rct.DataType => DataType.fromNamed(dt, bound).node
      case rct.FunType(a, b) => FunType(fromNamed(a, bound), fromNamed(b, bound))
      case rct.DepFunType(x: rct.NatIdentifier, t) => NatFunType(fromNamed(t, bound + x))
      case rct.DepFunType(x: rct.DataTypeIdentifier, t) => DataFunType(fromNamed(t, bound + x))
      case rct.DepFunType(_, _) => ???
      case rct.TypePlaceholder | rct.TypeIdentifier(_) =>
        throw new Exception(s"did not expect $t")
    })
  }

  def toNamed(t: Type, bound: Expr.Bound = Expr.Bound.empty): rct.Type = {
    t.node match {
      case dt: DataTypeNode[Nat, DataType] => DataType.toNamed(DataType(dt), bound)
      case FunType(a, b) => rct.FunType(toNamed(a, bound), toNamed(b, bound))
      case NatFunType(t) =>
        val i = rct.NatIdentifier(s"n${bound.nat.size}", isExplicit = true)
        rct.DepFunType[rct.NatKind, rct.Type](i, toNamed(t, bound + i))
      case DataFunType(t) =>
        val i = rct.DataTypeIdentifier(s"n${bound.data.size}", isExplicit = true)
        rct.DepFunType[rct.DataKind, rct.Type](i, toNamed(t, bound + i))
    }
  }
}

object DataType {
  def fromNamed(dt: rct.DataType, bound: Expr.Bound = Expr.Bound.empty): DataType = {
    DataType(dt match {
      case i: rct.DataTypeIdentifier => DataTypeVar(bound.indexOf(i))
      case s: rct.ScalarType => ScalarType(s)
      case rct.NatType => NatType
      case rct.VectorType(s, et) => VectorType(Nat.fromNamed(s, bound), fromNamed(et, bound))
      case rct.IndexType(s) => IndexType(Nat.fromNamed(s, bound))
      case rct.PairType(dt1, dt2) => PairType(fromNamed(dt1, bound), fromNamed(dt2, bound))
      case rct.ArrayType(s, et) => ArrayType(Nat.fromNamed(s, bound), fromNamed(et, bound))
      case _: rct.DepArrayType | _: rct.DepPairType[_] |
           _: rct.NatToDataApply | _: rct.FragmentType =>
        throw new Exception(s"did not expect $dt")
    })
  }

  def toNamed(dt: DataType, bound: Expr.Bound = Expr.Bound.empty): rct.DataType = {
    dt.node match {
      case DataTypeVar(index) => bound.data(index)
      case ScalarType(s) => s
      case NatType => rct.NatType
      case VectorType(s, et) => rct.VectorType(Nat.toNamed(s, bound), toNamed(et, bound))
      case IndexType(s) => rct.IndexType(Nat.toNamed(s, bound))
      case PairType(dt1, dt2) => rct.PairType(toNamed(dt1, bound), toNamed(dt2, bound))
      case ArrayType(s, et) => rct.ArrayType(Nat.toNamed(s, bound), toNamed(et, bound))
    }
  }
}

object TypePattern {
  def fromType(t: Type): TypePattern = ???
}

object DataTypePattern {
  def fromDataType(dt: DataType): DataTypePattern = ???
}

sealed trait TypeNode[+T, +N, +DT]
// final case class TypeVar(index: Int) extends TypeNode[Nothing]
final case class FunType[T](inT: T, outT: T) extends TypeNode[T, Nothing, Nothing]
final case class NatFunType[T](t: T) extends TypeNode[T, Nothing, Nothing]
final case class DataFunType[T](t: T) extends TypeNode[T, Nothing, Nothing]

sealed trait DataTypeNode[+N, +DT] extends TypeNode[Nothing, N, DT]
final case class DataTypeVar(index: Int) extends DataTypeNode[Nothing, Nothing]
final case class ScalarType(s: rct.ScalarType) extends DataTypeNode[Nothing, Nothing]
case object NatType extends DataTypeNode[Nothing, Nothing]
final case class VectorType[N, DT](size: N, elemType: DT) extends DataTypeNode[N, DT]
final case class IndexType[N](size: N) extends DataTypeNode[N, Nothing]
final case class PairType[DT](dt1: DT, dt2: DT) extends DataTypeNode[Nothing, DT]
final case class ArrayType[N, DT](size: N, elemType: DT) extends DataTypeNode[N, DT]