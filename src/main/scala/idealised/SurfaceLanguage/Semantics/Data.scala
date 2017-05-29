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

final case class IndexData(n: Nat) extends Data(IndexType(n.max))

final case class VectorData(v: Seq[ScalarData]) extends Data(VectorType(v.length, v.head.dataType))
