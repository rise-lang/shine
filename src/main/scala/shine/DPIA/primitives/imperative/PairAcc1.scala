package shine.DPIA.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.accPrimitive

@accPrimitive
final case class PairAcc1(dt1: DataType,
                          dt2: DataType,
                          pair: Phrase[AccType]
                         ) extends AccPrimitive {
  pair :: accT(dt1 x dt2)
  override val t: AccType = accT(dt1)

  override def eval(s: Store): AccIdentifier = {
    OperationalSemantics.eval(s, pair) match {
      case r: PairIdentifier => r.fst
      case _ => throw new Exception("This should not happen")
    }
  }
}
