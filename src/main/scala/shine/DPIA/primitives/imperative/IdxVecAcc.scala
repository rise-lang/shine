package shine.DPIA.primitives.imperative

import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.accPrimitive

@accPrimitive
final case class IdxVecAcc(n: Nat,
                           st: ScalarType,
                           index: Phrase[ExpType],
                           vector: Phrase[AccType]
                          ) extends AccPrimitive {
  index :: expT(idx(n), read)
  vector :: accT(vec(n, st))
  override val t: AccType = accT(st)

  override def eval(s: Store): AccIdentifier = {
    val vectorE = OperationalSemantics.eval(s, vector)
    val indexE = OperationalSemantics.eval(s, index) match {
      case IntData(i) => i
      case _ => throw new Exception("This should not happen")
    }
    VectorAccessIdentifier(vectorE, indexE)
  }
}
