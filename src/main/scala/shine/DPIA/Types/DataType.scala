package shine.DPIA.Types

//
import arithexpr.arithmetic.BigSum
import rise.core.types.DataType._

import scala.annotation.tailrec
//import rise.core.types.{Fragment, MatrixLayout}
//import shine.DPIA
//import shine.DPIA.{Nat, NatIdentifier}
//
//sealed trait DataType
//
//sealed trait ComposedType extends DataType
//
//sealed trait BasicType extends DataType
//
//sealed trait ScalarType extends BasicType
//
///**
//  * Represents a CUDA-fragment which represents a tile of a matrix which is stored in registers of a warp. <br>
//  * Fragments of kind `Accumulator` does not have a layout. So the `layout` of fragments of kind `Accumulator`
//  * can be ignored.
//  * @param rows         number of rows
//  * @param columns      number of columns
//  * @param layers           third dimension which is used in the MMA operation
//  * @param dataType     dataType of the elements
//  * @param fragmentKind kind of the fragment {@link FragmentKind}
//  * @param layout       layout of the fragment {@link MatrixLayout}
//  */
//final case class FragmentType(rows: Nat,
//                              columns: Nat,
//                              layers: Nat,
//                              dataType: DataType,
//                              fragmentKind: Fragment,
//                              layout: MatrixLayout) extends BasicType {
//  override def toString: String =
//    s"Fragment[$rows,$columns,$layers,$dataType,$fragmentKind,$layout]"
//
//  override def equals(o: Any): Boolean = {
//    o match {
//      case f: FragmentType =>
//        f.fragmentKind match {
//          case Fragment.Accumulator =>
//            f.rows.equals(rows) && f.columns.equals(columns) && f.layers.equals(layers) && f.dataType.equals(dataType)
//          case _ =>
//            f.rows.equals(rows) && f.columns.equals(columns) && f.layers.equals(layers) && f.dataType.equals(dataType) &&
//              f.fragmentKind.equals(fragmentKind) && f.layout.equals(layout)
//        }
//      case _ => false
//    }
//  }
//}
//
//object bool extends ScalarType { override def toString: String = "bool" }
//
//object int extends ScalarType { override def toString: String = "int" }
//
//object i8 extends ScalarType { override def toString: String = "i8" }
//object i16 extends ScalarType { override def toString: String = "i16" }
//object i32 extends ScalarType { override def toString: String = "i32" }
//object i64 extends ScalarType { override def toString: String = "i64" }
//
//object u8 extends ScalarType { override def toString: String = "u8" }
//object u16 extends ScalarType { override def toString: String = "u16" }
//object u32 extends ScalarType { override def toString: String = "u32" }
//object u64 extends ScalarType { override def toString: String = "u64" }
//
//object f16 extends ScalarType { override def toString: String = "f16" }
//object f32 extends ScalarType { override def toString: String = "f32" }
//object f64 extends ScalarType { override def toString: String = "f64" }
//
//object NatType extends ScalarType { override def toString: String = "nat" }
//
//final case class IndexType(size: Nat) extends BasicType {
//  override def toString: String = s"idx($size)"
//}
//
//final case class ArrayType(size: Nat, elemType: DataType) extends ComposedType {
//  override def toString: String = s"$size.$elemType"
//}
//
//// FIXME: this should be a backend extension
//final case class OpaqueType(name: String) extends DataType {
//  override def toString: String = name
//}
//
//// FIXME: this should be a backend extension
//final case class ManagedBufferType(dt: DataType) extends ComposedType {
//  override def toString: String = s"managed($dt)"
//}
//
//final case class DepArrayType private (size: Nat, elemFType: NatToData)
//  extends ComposedType
//{
//  override def toString: String = s"$size.$elemFType"
//
//  override def equals(obj: Any): Boolean = {
//    obj match {
//      case DepArrayType(s2, elemT2) => size == s2 && elemT2 == elemFType
//      case _ => false
//    }
//  }
//}
//
//final case class DepPairType(x:NatIdentifier, elemT:DataType)
//  extends ComposedType {
//  override def toString: String = s"($x:nat ** $elemT)"
//
//  override def equals(other: Any): Boolean =other match {
//    case DepPairType(x2, elemT2) =>
//      this.elemT == DataType.substitute(this.x, x2, elemT2)
//    case _ => false
//  }
//}
//
//
//final case class PairType(fst: DataType, snd: DataType) extends ComposedType {
//  override def toString: String = s"($fst x $snd)"
//}
//
//sealed case class VectorType(size: Nat, elemType: DataType)
//  extends BasicType
//{
//  override def toString: String = s"<$size>$elemType"
//}
//
//object vec {
//  @inline
//  def apply(size: Nat, elemType: DataType): VectorType =
//    VectorType(size, elemType)
//}
//
//final class NatToDataApply(val f: NatToData, val n: Nat) extends DataType {
//  override def toString: String = s"$f($n)"
//}
//
//object NatToDataApply {
//  def apply(f: NatToData, n: Nat): DataType = f match {
//    case l: NatToDataLambda     => l.apply(n)
//    case i: NatToDataIdentifier => new NatToDataApply(i, n)
//  }
//
//  def unapply(arg: NatToDataApply): Option[(NatToData, Nat)] =
//    Some((arg.f, arg.n))
//}
//
//final case class DataTypeIdentifier(name: String) extends DataType {
//  override def toString: String = name
//}
//
object DataType {
//
//  def substitute[T <: DataType](dt: DataType, `for`: DataType, in: T): T = {
//    if (`for` == in) {
//      dt.asInstanceOf[T]
//    } else {
//      (in match {
//        case _: BasicType | _: DataTypeIdentifier => in
//        case a: ArrayType =>
//          ArrayType(a.size, substitute(dt, `for`, a.elemType))
//        case r: PairType =>
//          PairType(substitute(dt, `for`, r.fst), substitute(dt, `for`, r.snd))
//      }).asInstanceOf[T]
//    }
//  }
//
//  def substitute[T <: DataType](ae: Nat, `for`: Nat, in: T): T = {
//    (in match {
//      case s: ScalarType => s
//      case i: IndexType =>
//        IndexType(ArithExpr.substitute(i.size, Map((`for`, ae))))
//      case a: ArrayType =>
//        ArrayType(ArithExpr.substitute(a.size, Map((`for`, ae))),
//          substitute(ae, `for`, a.elemType))
//      case f: FragmentType =>
//        FragmentType(ArithExpr.substitute(f.rows, Map((`for`, ae))),
//          ArithExpr.substitute(f.columns, Map((`for`, ae))),
//          ArithExpr.substitute(f.layers, Map((`for`, ae))),
//          substitute(ae, `for`, f.dataType), f.fragmentKind, f.layout)
//      case a: DepArrayType =>
//        val subMap = Map((`for`, ae))
//        val newSize = ArithExpr.substitute(a.size, subMap)
//        val newElemFType = substitute(ae, `for`, a.elemFType)
//        DepArrayType(newSize, newElemFType)
//      case v: VectorType =>
//        VectorType(ArithExpr.substitute(v.size, Map((`for`, ae))), v.elemType)
//      case r: PairType =>
//        PairType(substitute(ae, `for`, r.fst), substitute(ae, `for`, r.snd))
//      case r: DepPairType =>
//        val newFst = `for` match {
//          case ident: DPIA.NatIdentifier if ident == r.x => ae.asInstanceOf[NatIdentifier]
//          case _ =>  r.x
//        }
//        val newSnd = substitute(ae, `for`, r.elemT)
//        DepPairType(newFst, newSnd)
//    }).asInstanceOf[T]
//  }
//
//  def substitute(ae: DPIA.Nat, `for`: DPIA.Nat, in: NatToData): NatToData = {
//    in match {
//      case i: NatToDataIdentifier => i
//      case NatToDataLambda(x, body) =>
//        NatToDataLambda(x, substitute(ae, `for`, body))
//    }
//  }
//
  import rise.core.types.{DataType, Nat}
  import rise.core.types._

  def getTotalNumberOfElements(dt: DataType): Nat = dt match {
    case _: ScalarType | _: FragmentType | _: IndexType | `NatType` | _: VectorType => 1
    case _: PairType => 1
    case _: DepPairType[_, _] => 1
    case ManagedBufferType(dt) => getTotalNumberOfElements(dt)
    case a: ArrayType => getTotalNumberOfElements(a.elemType) * a.size
    case a: DepArrayType =>
      a.fdt match {
        case NatToDataLambda(x, body) =>
          BigSum(from = 0, upTo = a.size - 1,
            `for` = x, `in` = getTotalNumberOfElements(body))
        case NatToDataIdentifier(_) =>
          throw new Exception("This should not happen")
      }
    case _: DataTypeIdentifier | _: NatToDataApply | _: OpaqueType =>
      throw new Exception("This should not happen")
  }

  @tailrec
  def getSize(dt: DataType): Nat = dt match {
    case _: IndexType | _: ScalarType | NatType => 1
    case _: PairType => 1 // TODO: is this correct?
    case _: DepPairType[_, _] => 1
    case VectorType(size, _) => size
    case ManagedBufferType(dt) => getSize(dt)
    case ArrayType(size, _) => size
    case DepArrayType(size, _) => size
    case _: DataTypeIdentifier | _: NatToDataApply | _: OpaqueType |
         _: FragmentType =>
      throw new Exception("This should not happen")
  }

  def getSizes(dt: DataType): Seq[Nat] = dt match {
    case ArrayType(size, elemType) => Seq(size) ++ getSizes(elemType)
    case DepArrayType(size, NatToDataLambda(_, elemType)) =>
      Seq(size) ++ getSizes(elemType) // TODO: is this correct?
    case _ => Seq(getSize(dt))
  }

  @scala.annotation.tailrec
  def getBaseDataType(dt: DataType): DataType = dt match {
    case _: ScalarType | _: IndexType | _: FragmentType | NatType | _: VectorType => dt
    case _: PairType => dt
    case _: DepPairType[_, _] => dt
    case _: DataTypeIdentifier => dt
    case ManagedBufferType(dt) => getBaseDataType(dt)
    case ArrayType(_, elemType) => getBaseDataType(elemType)
    case DepArrayType(_, NatToDataLambda(_, elemType)) =>
      getBaseDataType(elemType)
    case DepArrayType(_, _) | _: NatToDataApply | _: OpaqueType =>
      throw new Exception("This should not happen")
  }

  @inline
  def idx(n: Nat): IndexType = IndexType(n)
}
