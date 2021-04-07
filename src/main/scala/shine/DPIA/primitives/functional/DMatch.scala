package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.imperative.DMatchI
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class DMatch(x: NatIdentifier,
                        elemT: DataType,
                        outT: DataType,
                        a: AccessType,
                        f: Phrase[`(nat)->:`[ExpType ->: ExpType]],
                        input: Phrase[ExpType]
                       ) extends ExpPrimitive with AccT {
  override val t: ExpType = expT(outT, a)

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext): Phrase[CommType] =
    // Turn the f imperative by means of forwarding the acceptor translation
    con(input)(λ(expT(DepPairType(x, elemT), read))(pair =>
      DMatchI(x, elemT, outT,
        _Λ_[NatKind]()((fst: NatIdentifier) =>
          λ(expT(DataType.substitute(fst, x, elemT), read))(snd =>
            acc(f(fst)(snd))(A)
          )), pair)))
}
