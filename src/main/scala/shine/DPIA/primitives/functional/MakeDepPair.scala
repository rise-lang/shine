package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.imperative.{MkDPairFstI, MkDPairSndAcc}
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class MakeDepPair(a: AccessType,
                             fst: NatIdentifier,
                             sndT: DataType,
                             snd: Phrase[ExpType]
                        ) extends ExpPrimitive with ContinuationTranslatable with AcceptorTranslatable {
  override val t: ExpType = expT(DepPairType(fst, sndT), a)

  def continuationTranslation(C: Phrase[ExpType ->: CommType])
                             (implicit context: TranslationContext): Phrase[CommType] = {
    // Allocate for the resulting dependent pair,
    // then imperatively write the first element,
    // acc-translate and write the second element
    // and call the continuation on the result
    // TODO(federico) - This is allocating eagerly. Make it allocate lazily by adding a suitable primitive:
    //  ideally Dmatch(..,..., MkDPair(x, y))
    // should not allocate
    `new`(t.dataType, outVar => {
      MkDPairFstI(fst, outVar.wr) `;`
        acc(snd)(MkDPairSndAcc(fst, sndT, outVar.wr)) `;`
        C(outVar.rd)
    })
  }

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext): Phrase[CommType] = {
    // We have the acceptor already, so simply write the first element and then
    // the second element in sequentially
    MkDPairFstI(fst, A) `;`
      acc(snd)(MkDPairSndAcc(fst, sndT, A))
  }
}
