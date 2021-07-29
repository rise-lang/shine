package rise.core.types

import arithexpr.arithmetic.BigSum
import rise.core.types.DataType._

import scala.annotation.tailrec

object DataTypeOps {
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
}
