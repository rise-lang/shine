package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics.{IndexData, NatData}
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class NatAsIndex(n: Nat,
                            e: Phrase[ExpType]
                           ) extends ExpPrimitive {
  e :: expT(NatType, read)
  override val t: ExpType = expT(idx(n), read)

  override def eval(s: OperationalSemantics.Store): OperationalSemantics.Data = {
    OperationalSemantics.eval(s, e) match {
      case NatData(m) => IndexData(m, n)
      case d => throw new Exception(s"Expected NatData but found $d.")
    }
  }
}
