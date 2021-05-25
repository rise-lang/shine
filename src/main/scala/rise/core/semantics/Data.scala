package rise.core.semantics

import rise.core.types._

sealed abstract class Data(val dataType: DataType)

final case class NatData(n: Nat) extends Data(NatType) {
  override def toString: String = s"($n: ${this.dataType})"
}

final case class IndexData(i: Nat, n: Nat) extends Data(IndexType(n)) {
  override def toString: String = s"($i: ${this.dataType})"
}

sealed abstract class ScalarData(override val dataType: ScalarType)
    extends Data(dataType)

final case class BoolData(b: Boolean) extends ScalarData(bool) {
  override def toString: String = b.toString
}

final case class ShortData(i: Short) extends ScalarData(i16) {
  override def toString: String = i.toString
}

final case class IntData(i: Int) extends ScalarData(int) {
  override def toString: String = i.toString
}

final case class FloatData(f: Float) extends ScalarData(f32) {
  override def toString: String = f"$f%.4f"
}

final case class DoubleData(d: Double) extends ScalarData(f64) {
  override def toString: String = f"$d%.6f"
}

final case class VectorData(v: Seq[ScalarData])
    extends Data(VectorType(v.length, v.head.dataType)) {
  override def toString: String = v.mkString("<", ",", ">")
}

final case class ArrayData(a: Seq[Data])
    extends Data(ArrayType(a.length, a.head.dataType)) {
  override def toString: String = a.mkString("[", ",", "]")
}

final case class PairData(p1: Data, p2: Data)
    extends Data(PairType(p1.dataType, p2.dataType)) {
  override def toString: String = s"($p1, $p2)"
}
