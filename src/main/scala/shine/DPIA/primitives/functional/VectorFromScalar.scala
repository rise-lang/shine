package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class VectorFromScalar(n: Nat,
                                  dt: ScalarType,
                                  arg: Phrase[ExpType]
                                 ) extends ExpPrimitive {
  arg :: expT(dt, read)
  override val t: ExpType = expT(vec(n, dt), read)
}
