package shine.DPIA.primitives.functional

import arithexpr.arithmetic.BigSum
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class DepJoin(n: Nat,
                         lenF: NatToNat,
                         dt: DataType,
                         array: Phrase[ExpType]
                        ) extends ExpPrimitive {
  array :: expT(n`.d`{ i => lenF(i) `.` dt }, read)
  override val t: ExpType = expT(BigSum(from = 0, upTo = n - 1, i => lenF(i))`.`dt, read)

  override def eval(s: Store): Data = {
    OperationalSemantics.eval(s, array) match {
      case ArrayData(outer) =>
        val arrays = outer.map {
          case ArrayData(inner) => inner
          case _ => throw new Exception("This should not happen")
        }
        ArrayData(arrays.flatten)

      case _ => throw new Exception("This should not happen")
    }
  }
}
