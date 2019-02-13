package idealised.DPIA.Types

import idealised.DPIA.{Nat, freshName}
import idealised.SurfaceLanguage
import idealised.SurfaceLanguage.NatIdentifier
import lift.arithmetic.{ArithExpr, BigSum, NamedVar, RangeAdd}

import scala.language.implicitConversions

sealed trait DataType

sealed trait ComposedType extends DataType

sealed trait BasicType extends DataType

sealed trait ScalarType extends BasicType

object bool extends ScalarType { override def toString: String = "bool" }

object int extends ScalarType { override def toString: String = "int" }

object float extends ScalarType { override def toString: String = "float" }

object double extends ScalarType { override def toString: String = "double" }

final case class IndexType(size: Nat) extends BasicType {
  override def toString: String = s"idx($size)"
}

final case class ArrayType(size: Nat, elemType: DataType) extends ComposedType {
  override def toString: String = s"$size.$elemType"
}

final case class DepArrayType(size:Nat, i: NatIdentifier, elemType: DataType) extends ComposedType {
  override def toString: String = s"$size.($i -> $elemType)"

  override def equals(that: Any): Boolean = that match {
    case DepArrayType(size_, i_, elemType_) =>
      val eq = size == size_ && elemType == DataType.substitute(i, `for`=i_, elemType_)
      eq
    case _ => false
  }
}

object DepArrayType {
  def apply(size: Nat, f: NatIdentifier => DataType): DepArrayType = {
    val newName = NamedVar(freshName(), RangeAdd(0, size, 1))
    DepArrayType(size, newName, f(newName))
  }
}

final case class RecordType(fst: DataType, snd: DataType) extends ComposedType {
  override def toString: String = s"($fst x $snd)"
}

sealed case class VectorType(size: Nat, elemType: ScalarType) extends BasicType {
  override def toString: String = s"$elemType$size"
}

object int2 extends VectorType(2, int)
object int3 extends VectorType(3, int)
object int4 extends VectorType(4, int)
object int8 extends VectorType(8, int)
object int16 extends VectorType(16, int)

object float2 extends VectorType(2, float)
object float3 extends VectorType(3, float)
object float4 extends VectorType(4, float)
object float8 extends VectorType(8, float)
object float16 extends VectorType(16, float)

final case class DataTypeIdentifier(name: String) extends DataType {
  override def toString: String = name
}

object ScalarType {
  implicit def apply(st: SurfaceLanguage.Types.ScalarType): ScalarType = {
    st match {
      case SurfaceLanguage.Types.bool => bool
      case SurfaceLanguage.Types.int => int
      case SurfaceLanguage.Types.float => float
      case SurfaceLanguage.Types.double => double
    }
  }
}

object DataType {

  def substitute[T <: DataType](dt: DataType, `for`: DataType, in: T): T = {
    if (`for` == in) {
      dt.asInstanceOf[T]
    } else {
      (in match {
        case _: BasicType => in
        case a: ArrayType => ArrayType(a.size, substitute(dt, `for`, a.elemType))
        case r: RecordType => RecordType(substitute(dt, `for`, r.fst), substitute(dt, `for`, r.snd))
      }).asInstanceOf[T]
    }
  }

  def substitute[T <: DataType](ae: Nat, `for`: Nat, in: T): T = {
    (in match {
      case i: IndexType => IndexType(ArithExpr.substitute(i.size, Map((`for`, ae))))
      case b: BasicType => b
      case a: ArrayType =>
        ArrayType(ArithExpr.substitute(a.size, Map((`for`, ae))),
          substitute(ae, `for`, a.elemType))
      case a: DepArrayType =>
        val subMap = Map((`for`,ae))
        DepArrayType(ArithExpr.substitute(a.size, subMap), ArithExpr.substitute(a.i, subMap).asInstanceOf[NatIdentifier], substitute(ae, `for`, `in`=a.elemType))
      case r: RecordType =>
        RecordType(substitute(ae, `for`, r.fst), substitute(ae, `for`, r.snd))
    }).asInstanceOf[T]
  }

  implicit def apply(dt: SurfaceLanguage.Types.DataType): DataType = {
    dt match {
      case bt: SurfaceLanguage.Types.BasicType => bt match {
        case st: SurfaceLanguage.Types.ScalarType => ScalarType(st)
        case vt: SurfaceLanguage.Types.VectorType => VectorType(vt.size, ScalarType(vt.elemType))
        case it: SurfaceLanguage.Types.IndexType => IndexType(it.size)
      }
      case ct: SurfaceLanguage.Types.ComposedType => ct match {
        case at: SurfaceLanguage.Types.ArrayType => ArrayType(at.size, DataType(at.elemType))
        case dat:SurfaceLanguage.Types.DepArrayType =>
          DepArrayType(dat.size, dat.elemType.x, DataType(dat.elemType.t))
        case tt: SurfaceLanguage.Types.TupleType =>
          assert(tt.elemTypes.size == 2)
          //noinspection ZeroIndexToHead
          RecordType(DataType(tt.elemTypes(0)), DataType(tt.elemTypes(1)))
      }
      case i: SurfaceLanguage.Types.DataTypeIdentifier => DataTypeIdentifier(i.name)
    }
  }

  def getLength(dt: DataType, tupleAccess: List[Nat]): Nat = dt match {
    case _: BasicType => 1
    case r: RecordType =>
      val t = tupleAccess.head
      val elemT = if (t == (1: Nat)) { r.fst } else if (t == (2: Nat)) { r.snd } else { throw new Exception("This should not happen") }
      getLength(elemT, tupleAccess.tail)
    case a: ArrayType => getLength(a.elemType, tupleAccess) * a.size
    case a: DepArrayType => BigSum(from = 0, upTo = a.size - 1, `for` = a.i, `in` = getLength(a.elemType, tupleAccess))
    case _: DataTypeIdentifier => throw new Exception("This should not happen")
  }

  def getLength(dt: DataType): Nat = dt match {
    case _: BasicType => 1
    case _: RecordType => 1
    case a: ArrayType => getLength(a.elemType) * a.size
    case a: DepArrayType => BigSum(from = 0, upTo = a.size - 1, `for` = a.i, `in` = getLength(a.elemType))
    case _: DataTypeIdentifier => throw new Exception("This should not happen")
  }

  def getSize(dt: DataType): Nat = dt match {
    case _: IndexType | _: ScalarType => 1
    case _: RecordType => 1 // TODO: is this correct?
    case VectorType(size, _) => size
    case ArrayType(size, _) => size
    case DepArrayType(size, _, _) => size
    case _: DataTypeIdentifier => throw new Exception("This should not happen")
  }

  def getSizes(dt: DataType): Seq[Nat] = dt match {
    case ArrayType(size, elemType) => Seq(size) ++ getSizes(elemType)
    case DepArrayType(size, _, elemType) => Seq(size) ++ getSizes(elemType) // TODO: is this correct?
    case _ => Seq(getSize(dt))
  }

  def getBaseDataType(dt: DataType): DataType = dt match {
    case _: BasicType => dt
    case _: RecordType => dt
    case _: DataTypeIdentifier => dt
    case ArrayType(_, elemType) => getBaseDataType(elemType)
    case DepArrayType(_, _, elemType) => getBaseDataType(elemType)
  }

  implicit class RecordTypeConstructor(dt1: DataType) {
    def x(dt2: DataType) = RecordType(dt1, dt2)
  }

  implicit class ArrayTypeConstructor(s: Nat) {
    def `.`(dt: DataType) = ArrayType(s, dt)
  }
}