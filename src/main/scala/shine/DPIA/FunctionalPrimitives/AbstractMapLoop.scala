package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._

abstract class AbstractMapLoop(override val n: Nat,
                               override val dt1: DataType,
                               override val dt2: DataType,
                               override val f: Phrase[ExpType ->: ExpType],
                               override val array: Phrase[ExpType])
  extends AbstractMap(n, dt1, dt2, f, array)
{
  def makeMapI(n: Nat, dt1: DataType, dt2: DataType,
               f: Phrase[ExpType ->: AccType ->: CommType],
               array: Phrase[ExpType],
               out: Phrase[AccType])
              (implicit context: TranslationContext): Phrase[CommType]

  def mapAcceptorTranslation(f: Phrase[ExpType ->: ExpType],
                             A: Phrase[AccType])
                            (implicit context: TranslationContext): Phrase[CommType] = {
    ???
  }
  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    con(array)(λ(expT(n`.`dt1, read))(x =>
      makeMapI(n, dt1, dt2,
        λ(expT(dt1, read))(x => λ(accT(dt2))(o => acc(f(x))(o))),
        x, A)))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    println("WARNING: map loop continuation translation allocates memory")
    // TODO should be removed
    `new`(n`.`dt2, λ(varT(n`.`dt2))(tmp =>
      acc(this)(tmp.wr) `;` C(tmp.rd) ))
  }
}
