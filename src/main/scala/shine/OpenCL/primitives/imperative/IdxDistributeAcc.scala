package shine.OpenCL.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA.{Nat, _}
import shine.OpenCL.ParallelismLevel
import shine.macros.Primitive.accPrimitive

@accPrimitive
final case class IdxDistributeAcc(m: Nat,
                                  n: Nat,
                                  stride: Nat,
                                  parallelismLevel: ParallelismLevel,
                                  dt: DataType,
                                  array: Phrase[AccType]
                                 ) extends AccPrimitive {
  array :: accT(m`.`dt)
  override val t: AccType = accT(n`.`dt)
}
