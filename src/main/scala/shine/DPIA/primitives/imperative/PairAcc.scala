package shine.DPIA.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.accPrimitive

@accPrimitive
final case class PairAcc(dt1: DataType,
                           dt2: DataType,
                           fst: Phrase[AccType],
                           snd: Phrase[AccType]
                        ) extends AccPrimitive {
  fst :: accT(dt1)
  snd :: accT(dt2)
  override val t: AccType = accT(dt1 x dt2)
}
