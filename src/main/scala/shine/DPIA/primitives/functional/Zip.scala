package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Zip(n: Nat,
                     dt1: DataType,
                     dt2: DataType,
                     access: AccessType,
                     e1: Phrase[ExpType],
                     e2: Phrase[ExpType]
                    ) extends ExpPrimitive {
  e1 :: expT(n`.`dt1, access)
  e2 :: expT(n`.`dt2, access)
  override val t: ExpType = expT(n`.`(dt1 x dt2), access)

  override def eval(s: Store): Data = {
    (OperationalSemantics.eval(s, e1), OperationalSemantics.eval(s, e2)) match {
      case (ArrayData(lhsE), ArrayData(rhsE)) =>
        ArrayData((lhsE zip rhsE) map { p =>
          PairData(p._1, p._2)
        })
      case _ => throw new Exception("This should not happen")
    }
  }
}
