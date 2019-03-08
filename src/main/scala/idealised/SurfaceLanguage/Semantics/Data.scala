package idealised.SurfaceLanguage.Semantics

import idealised.SurfaceLanguage.Nat
import idealised.SurfaceLanguage.Types._

sealed abstract class Data(val dataType: DataType)

sealed abstract class ScalarData(override val dataType: ScalarType) extends Data(dataType)

final case class ArrayData(a: Seq[Data]) extends Data(ArrayType(a.length, a.head.dataType))

final case class TupleData(t: Data*) extends Data(TupleType(t.map(_.dataType):_*))

final case class BoolData(b: Boolean) extends ScalarData(bool)

final case class IntData(i: Int) extends ScalarData(int)

final case class FloatData(f: Float) extends ScalarData(float)

final case class DoubleData(d: Double) extends ScalarData(double)

final case class IndexData(n: Nat, indexType: IndexType) extends Data(indexType)

object IndexData {
  def apply(n: Nat): IndexData = IndexData(n, IndexType(n.max + 1))
}

final case class VectorData(v: Seq[ScalarData]) extends Data(VectorType(v.length, v.head.dataType))

//TODO where should this go? This is describing a value of type DataType, but is not allowed to be passed to LiteralExpr for example
final case class NatData(n: Nat)
