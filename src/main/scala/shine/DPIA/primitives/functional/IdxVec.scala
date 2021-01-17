package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
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
                       ) extends ExpPrimitive with ContinuationTranslatable with AcceptorTranslatable {
  index :: expT(idx(n), read)
  vector :: expT(vec(n, st), read)
  override val t: ExpType = expT(st, read)

  override def eval(s: Store): Data = {
    (OperationalSemantics.eval(s, vector), OperationalSemantics.eval(s, index)) match {
      case (VectorData(xs), IntData(i)) => xs(i)
      case _ => throw new Exception("This should not happen")
    }
  }

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext): Phrase[CommType] =
    con(vector)(λ(expT(vec(n, st), read))(x =>
      A :=| st | IdxVec(n, st, index, x)))

  def continuationTranslation(C: Phrase[ExpType ->: CommType])
                             (implicit context: TranslationContext): Phrase[CommType] =
    con(vector)(λ(expT(vec(n, st), read))(e =>
      C(IdxVec(n, st, index, e))))
}
