package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._

final case class IterateStream(
  override val n: Nat,
  override val dt1: DataType,
  override val dt2: DataType,
  override val f: Phrase[ExpType ->: ExpType],
  override val array: Phrase[ExpType]
) extends AbstractMap(n, dt1, dt2, f, array)
{
  override def makeMap: (Nat, DataType, DataType,
    Phrase[ExpType ->: ExpType], Phrase[ExpType]) => AbstractMap = IterateStream

  override def acceptorTranslation(A: Phrase[AccType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = {
    import shine.DPIA.Compilation.TranslationToImperative._

    val fI = λ(expT(dt1, read))(x => λ(accT(dt2))(o => acc(f(x))(o)))
    val i = NatIdentifier(freshName("i"))
    str(array)(fun((i: NatIdentifier) ->:
      (expT(dt1, read) ->: (comm: CommType)) ->: (comm: CommType)
    )(next =>
      comment("iterateStream")`;`
      forNat(n, i =>
        streamNext(next, i, fun(expT(dt1, read))(x => fI(x)(A `@` i))))
    ))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = ???
}
