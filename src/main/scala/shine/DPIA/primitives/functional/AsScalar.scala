package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class AsScalar(n: Nat,
                          m: Nat,
                          dt: ScalarType,
                          access: AccessType,
                          array: Phrase[ExpType]
                         ) extends ExpPrimitive {
  array :: expT(n `.` vec(m, dt), access)
  override val t: ExpType = expT((n * m) `.` dt, access)
}
