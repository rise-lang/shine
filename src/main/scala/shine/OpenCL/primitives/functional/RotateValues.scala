package shine.OpenCL.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class RotateValues(a: AddressSpace,
                              n: Nat,
                              sz: Nat,
                              dt: DataType,
                              write: Phrase[ExpType ->: ExpType],
                              input: Phrase[ExpType]
                             ) extends ExpPrimitive {
  write :: expT(dt, read) ->: expT(dt, shine.DPIA.Types.write)
  input :: expT((n - 1 + sz)`.`dt, read)
  override val t: ExpType = expT(n`.`(sz`.`dt), read)

  override def eval(s: Store): Data = {
    import shine.DPIA.primitives.functional.Slide
    Slide(n, sz, 1, dt, input).eval(s)
  }
}