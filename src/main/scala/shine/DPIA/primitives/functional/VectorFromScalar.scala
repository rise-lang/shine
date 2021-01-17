package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class VectorFromScalar(n: Nat,
                                  dt: ScalarType,
                                  arg: Phrase[ExpType]
                                 ) extends ExpPrimitive with ContinuationTranslatable with AcceptorTranslatable {
  arg :: expT(dt, read)
  override val t: ExpType = expT(vec(n, dt), read)

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext): Phrase[CommType] =
    con(arg)(λ(expT(dt, read))(e =>
      A :=|VectorType(n, dt)| VectorFromScalar(n, dt, e)))

  def continuationTranslation(C: Phrase[ExpType ->: CommType])
                             (implicit context: TranslationContext): Phrase[CommType] =
    con(arg)(λ(expT(dt, read))(e =>
      C(VectorFromScalar(n, dt, e)) ))
}
