package shine.DPIA.primitives.functional

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

import scala.language.reflectiveCalls

@expPrimitive
final case class IdxVec(n: Nat,
                        st: ScalarType,
                        index: Phrase[ExpType],
                        vector: Phrase[ExpType]
                       ) extends ExpPrimitive {
  index :: expT(idx(n), read)
  vector :: expT(vec(n, st), read)
  override val t: ExpType = expT(st, read)

  override def eval(s: Store): Data = {
    (OperationalSemantics.eval(s, vector), OperationalSemantics.eval(s, index)) match {
      case (VectorData(xs), IntData(i)) => xs(i)
      case _ => throw new Exception("This should not happen")
    }
  }
}
