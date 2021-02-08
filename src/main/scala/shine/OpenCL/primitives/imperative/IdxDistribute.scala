package shine.OpenCL.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL.ParallelismLevel
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class IdxDistribute(m: Nat,
                               n: Nat,
                               stride: Nat,
                               parallelismLevel: ParallelismLevel,
                               dt: DataType,
                               array: Phrase[ExpType]
                              ) extends ExpPrimitive {
  array :: expT(m`.`dt, read)
  override val t: ExpType = expT(n`.`dt, read)
}
