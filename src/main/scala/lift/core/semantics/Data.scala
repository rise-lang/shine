package lift.core.semantics

import lift.core.Literal
import lift.core.types._

import scala.language.implicitConversions

sealed abstract class Data(val dataType: DataType)

sealed abstract class ScalarData(override val dataType: ScalarType) extends Data(dataType)

final case class ArrayData(a: Seq[Data]) extends Data(ArrayType(a.length, a.head.dataType))

final case class TupleData(t: Data*) extends Data(TupleType(t.map(_.dataType):_*))

final case class BoolData(b: Boolean) extends ScalarData(bool)

final case class IntData(i: Int) extends ScalarData(int)

final case class FloatData(f: Float) extends ScalarData(float)

final case class DoubleData(d: Double) extends ScalarData(double)

final case class VectorData(v: Seq[ScalarData]) extends Data(VectorType(v.length, v.head.dataType))

object Conversions {
  implicit def fromInt(x:Int):Literal = Literal(IntData(x))
}