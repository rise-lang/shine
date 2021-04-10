package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Join(n: Nat,
                      m: Nat,
                      w: AccessType,
                      dt: DataType,
                      array: Phrase[ExpType]
                     ) extends ExpPrimitive {
  array :: expT(n`.`(m`.`dt), w)
  override val t: ExpType = expT((n * m)`.`dt, w)

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
