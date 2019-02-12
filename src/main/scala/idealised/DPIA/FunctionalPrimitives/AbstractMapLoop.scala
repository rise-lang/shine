package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.{TranslationToImperative, TranslationContext}
import idealised.DPIA._
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._

abstract class AbstractMapLoop(n: Nat,
                               dt1: DataType,
                               dt2: DataType,
                               f: Phrase[ExpType -> ExpType],
                               array: Phrase[ExpType])
  extends AbstractMap(n, dt1, dt2, f, array)
{
  def makeMapI(n: Nat, dt1: DataType, dt2: DataType,
               f: Phrase[ExpType -> (AccType -> CommandType)],
               array: Phrase[ExpType],
               out: Phrase[AccType])
              (implicit context: TranslationContext): Phrase[CommandType]
  
  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._

    con(array)(λ(exp"[$n.$dt1]")(x =>
      makeMapI(n, dt1, dt2, λ(exp"[$dt1]")(x => λ(acc"[$dt2]")(o => acc(f(x))(o))), x, A)(context)))
  }

  override def continuationTranslation(C: Phrase[ExpType -> CommandType])
                                      (implicit context: TranslationContext): Phrase[CommandType] = {
    import TranslationToImperative._

    `new`(dt"[$n.$dt2]", λ(exp"[$n.$dt2]" x acc"[$n.$dt2]")(tmp =>
      acc(this)(tmp.wr) `;` C(tmp.rd) ))
  }
}
