package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Snd(dt1: DataType,
                     dt2: DataType,
                     pair: Phrase[ExpType]
                    ) extends ExpPrimitive {
  pair :: expT(dt1 x dt2, read)
  override val t: ExpType = expT(dt2, read)

  override def eval(s: Store): Data = {
    OperationalSemantics.eval(s, pair) match {
      case r: PairData => r.snd
      case _ => throw new Exception("This should not happen")
    }
  }
}
