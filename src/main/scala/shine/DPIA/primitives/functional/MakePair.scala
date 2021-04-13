package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class MakePair(dt1: DataType,
                          dt2: DataType,
                          access: AccessType,
                          fst: Phrase[ExpType],
                          snd: Phrase[ExpType]
                         ) extends ExpPrimitive {
  fst :: expT(dt1, access)
  snd :: expT(dt2, access)
  override val t: ExpType = expT(dt1 x dt2, access)

  override def eval(s: Store): Data = {
    PairData(
      OperationalSemantics.eval(s, fst),
      OperationalSemantics.eval(s, snd))
  }
}
