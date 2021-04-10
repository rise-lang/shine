package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Unzip(n: Nat,
                       dt1: DataType,
                       dt2: DataType,
                       access: AccessType,
                       e: Phrase[ExpType]
                      ) extends ExpPrimitive {
  e :: expT(n`.`(dt1 x dt2), access)
  override val t: ExpType = expT((n`.`dt1) x (n`.`dt2), access)

  override def eval(s: Store): Data = {
    OperationalSemantics.eval(s, e) match {
      case ArrayData(xs) =>
        val (lhs, rhs) = xs.foldLeft((Vector[Data](), Vector[Data]())) {
          case (vs: (Vector[Data], Vector[Data]), p: PairData) =>
            (vs._1 :+ p.fst, vs._2 :+ p.snd)
          case _ => throw new Exception("This should not happen")
        }
        PairData(ArrayData(lhs), ArrayData(rhs))
      case _ => throw new Exception("This should not happen")
    }
  }
}
