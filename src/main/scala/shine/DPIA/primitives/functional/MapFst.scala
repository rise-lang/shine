package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class MapFst(w: AccessType,
                        dt1: DataType,
                        dt2: DataType,
                        dt3: DataType,
                        f: Phrase[ExpType ->: ExpType],
                        record: Phrase[ExpType]
                       ) extends ExpPrimitive {
  f :: expT(dt1, w) ->: expT(dt3, w)
  record :: expT(dt1 x dt2, w)
  override val t: ExpType = expT(dt3 x dt2, w)

  override def eval(s: Store): Data = {
    val fE = OperationalSemantics.eval(s, f)
    OperationalSemantics.eval(s, record) match {
      case r: PairData =>
        PairData(OperationalSemantics.eval(s, fE(Literal(r.fst))), r.snd)
      case _ => throw new Exception("This should not happen")
    }
  }
}
