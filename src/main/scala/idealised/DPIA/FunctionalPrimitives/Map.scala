package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import idealised.DPIA.DSL._
import idealised.DPIA.ImperativePrimitives.MapRead
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA._

final case class Map(n: Nat,
                     dt1: DataType,
                     dt2: DataType,
                     f: Phrase[ExpType ->: ExpType],
                     array: Phrase[ExpType])
  extends AbstractMap(n, dt1, dt2, f, array)
{
  override def makeMap: (Nat, DataType, DataType, Phrase[ExpType ->: ExpType], Phrase[ExpType]) => AbstractMap = Map

  override def mapAcceptorTranslation(g: Phrase[ExpType ->: ExpType], A: Phrase[AccType])
                                     (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    mapAcc(g o f, array)(A)
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    con(array)(λ(exp"[$n.$dt1, $read]")(x =>
      C(MapRead(n, dt1, dt2,
        fun(exp"[$dt1, $read]")(a =>
          fun(exp"[$dt2, $read]" ->: (comm: CommType))(cont =>
            con(f(a))(fun(exp"[$dt2, $read]")(b => Apply(cont, b))))),
        x))))
  }
}
