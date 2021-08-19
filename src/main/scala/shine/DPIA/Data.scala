package shine.DPIA

import rise.core.types._
import rise.core.types.DataType._

sealed abstract class Data(val dataType: DataType)

final case class IndexData(i: Nat, n: Nat) extends Data(IndexType(n))

final case class NatData(n: Nat) extends Data(NatType)

final case class BoolData(b: Boolean) extends Data(bool)

final case class IntData(i: Int) extends Data(int) {
  override def toString: String = i.toString
}

final case class NatAsIntData(n: Nat) extends Data(int)

final case class FloatData(f: Float) extends Data(f32) {
  override def toString: String = f.toString + "f"
}

final case class DoubleData(d: Double) extends Data(f64) {
  override def toString: String = d.toString
}

final case class VectorData(a: Vector[Data]) extends Data(VectorType(a.length, a.head.dataType match {
  case b: ScalarType => b
  case _ => throw new Exception("This should not happen")
}))

final case class ArrayData(a: Vector[Data]) extends Data(ArrayType(a.length, a.head.dataType))

final case class PairData(fst: Data, snd: Data) extends Data(PairType(fst.dataType, snd.dataType))
