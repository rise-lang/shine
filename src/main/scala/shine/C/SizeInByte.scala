package shine.C

import arithexpr.arithmetic.{BigSum, Cst, GoesToRange}
import shine.DPIA.{Nat, NatIdentifier}
import shine.DPIA.Types.{ArrayType, DataType, DataTypeIdentifier, DepArrayType, DepPairType, IndexType, NatToDataApply, NatToDataIdentifier, NatToDataLambda, PairType, ScalarType, VectorType}

object SizeInByte {
  def apply(dt: DataType): SizeInByte = dt match {
    case s: ScalarType => s match {
      case shine.DPIA.Types.bool => SizeInByte(1)
      case shine.DPIA.Types.int | shine.DPIA.Types.NatType => SizeInByte(4)
      case shine.DPIA.Types.u8 | shine.DPIA.Types.i8 =>
        SizeInByte(1)
      case shine.DPIA.Types.u16 | shine.DPIA.Types.i16 | shine.DPIA.Types.f16 =>
        SizeInByte(2)
      case shine.DPIA.Types.u32 | shine.DPIA.Types.i32 | shine.DPIA.Types.f32 =>
        SizeInByte(4)
      case shine.DPIA.Types.u64 | shine.DPIA.Types.i64 | shine.DPIA.Types.f64 =>
        SizeInByte(8)
    }
    case _: IndexType => SizeInByte(4) // == sizeof(int)
    case v: VectorType => SizeInByte(v.elemType) * v.size
    case r: PairType => SizeInByte(r.fst) + SizeInByte(r.snd)
    case a: ArrayType => SizeInByte(a.elemType) * a.size
    case a: DepArrayType =>
      a.elemFType match {
        case NatToDataLambda(x, body) =>
          SizeInByte(BigSum(Cst(0), a.size - 1, `for`=x, in=SizeInByte(body).value))
        case _: NatToDataIdentifier =>
          throw new Exception("This should not happen")
      }
    case DepPairType(x, dt) => x match {
      case _: NatIdentifier =>
        SizeInByte(shine.DPIA.Types.NatType) + SizeInByte(dt)
      case _ => ???
    }
    case _: NatToDataApply =>  throw new Exception("This should not happen")
    case _: DataTypeIdentifier => throw new Exception("This should not happen")
  }
}

final case class SizeInByte(value: Nat) {
  def *(rhs: Nat) = SizeInByte(value * rhs)
  def +(rhs: SizeInByte) = SizeInByte(value + rhs.value)

  override def toString = s"$value bytes"
}
