package shine.OpenCL.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.functional.Slide
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class CircularBuffer(a: AddressSpace,
                                n: Nat,
                                alloc: Nat,
                                sz: Nat,
                                dt1: DataType,
                                dt2: DataType,
                                load: Phrase[ExpType ->: ExpType],
                                input: Phrase[ExpType]
                               ) extends ExpPrimitive {
  load :: expT(dt1, read) ->: expT(dt2, write)
  input :: expT((n - 1 + sz)`.`dt1, read)
  override val t: ExpType = expT(n`.`(sz`.`dt2), read)

  override def eval(s: Store): Data = {
    import shine.DPIA.primitives.functional.Map
    Slide(n, sz, 1, dt2, Map((n - 1 + sz), dt1, dt2, read, load, input)).eval(s)
  }
}