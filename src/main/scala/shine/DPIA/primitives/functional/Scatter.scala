package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Scatter(n: Nat, m: Nat, dt: DataType,
                         indices: Phrase[ExpType],
                         input: Phrase[ExpType]
                        ) extends ExpPrimitive {
  indices :: expT(n`.`idx(m), read)
  input :: expT(n`.`dt, write)
  override val t: ExpType = expT(m`.`dt, write)
}
