package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._

final case class MapStream(
  override val n: Nat,
  override val dt1: DataType,
  override val dt2: DataType,
  override val f: Phrase[ExpType ->: ExpType],
  override val array: Phrase[ExpType]
) extends AbstractMap(n, dt1, dt2, f, array)
{
  override def makeMap: (Nat, DataType, DataType,
    Phrase[ExpType ->: ExpType], Phrase[ExpType]) => AbstractMap = MapStream

  override def streamTranslation(
    C: Phrase[`(nat)->:`[(ExpType ->: CommType) ->: CommType] ->: CommType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = {
    import shine.DPIA.Compilation.TranslationToImperative._

    val i = NatIdentifier(freshName("i"))
    str(array)(fun((i: NatIdentifier) ->:
      (expT(dt1, read) ->: (comm: CommType)) ->: (comm: CommType)
    )(next =>
      C(nFun(i =>
        fun(expT(dt2, read) ->: (comm: CommType))(k =>
          streamNext(next, i, fun(expT(dt1, read))(x =>
            con(f(x))(k)
          ))
      ), arithexpr.arithmetic.RangeAdd(0, n, 1)))
    ))
  }

  override def acceptorTranslation(A: Phrase[AccType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = ???

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(
    implicit context: TranslationContext
  ): Phrase[CommType] = ???
}
