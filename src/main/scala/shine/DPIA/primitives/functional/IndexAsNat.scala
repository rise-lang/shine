package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class IndexAsNat(n: Nat,
                            e: Phrase[ExpType]
                           ) extends ExpPrimitive {
  e :: expT(idx(n), read)
  override val t: ExpType = expT(NatType, read)

  override def eval(s: OperationalSemantics.Store): OperationalSemantics.Data = {
    OperationalSemantics.eval(s, e) match {
      case IndexData(i, _) => NatData(i)
      case d => throw new Exception(s"Expected IndexData but found $d.")
    }
  }
}
