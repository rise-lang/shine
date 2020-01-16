package rise.core.semantics

import rise.core.Nat
import rise.core.types._

sealed abstract class Data(val dataType: DataType)

final case class NatData(n: Nat) extends Data(NatType)

final case class IndexData(i: Nat, n: Nat) extends Data(IndexType(n))

sealed abstract class ScalarData(override val dataType: ScalarType)
    extends Data(dataType)

final case class BoolData(b: Boolean) extends ScalarData(bool)

final case class IntData(i: Int) extends ScalarData(int)

final case class FloatData(f: Float) extends ScalarData(f32)

final case class DoubleData(d: Double) extends ScalarData(f64)

final case class VectorData(v: Seq[ScalarData])
    extends Data(VectorType(v.length, v.head.dataType))

final case class ArrayData(a: Seq[Data])
    extends Data(ArrayType(a.length, a.head.dataType))

final case class PairData(p1: Data, p2: Data)
    extends Data(PairType(p1.dataType, p2.dataType))
