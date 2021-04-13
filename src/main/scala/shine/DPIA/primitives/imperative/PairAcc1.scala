package shine.DPIA.primitives.imperative

import shine.DPIA.Phrases._
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
}
