package shine.cuda.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL.ParallelismLevel
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Map(level: ParallelismLevel,
                     dim: Int)
                    (val n: Nat,
                     val dt1: DataType,
                     val dt2: DataType,
                     val f: Phrase[ExpType ->: ExpType],
                     val array: Phrase[ExpType]
                    ) extends ExpPrimitive {
  f :: expT(dt1, read) ->: expT(dt2, write)
  array :: expT(n `.` dt1, read)
  override val t: ExpType = expT(n `.` dt2, write)

  def unwrap: (Nat, DataType, DataType, Phrase[ExpType ->: ExpType], Phrase[ExpType]) =
    (n, dt1, dt2, f, array)
}
