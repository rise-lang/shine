package idealised.OpenMP.FunctionalPrimitives

import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.FunctionalPrimitives.AbstractMapLoop
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenMP.IntermediatePrimitives.MapParI

//noinspection TypeAnnotation
final case class MapPar(n: Nat,
                        dt1: DataType,
                        dt2: DataType,
                        f: Phrase[ExpType -> ExpType],
                        array: Phrase[ExpType])
  extends AbstractMapLoop(n, dt1, dt2, f, array)
{
  override def makeMap = MapPar
  override def makeMapI(n: Nat, dt1: DataType, dt2: DataType,
                        f: Phrase[->[ExpType, ->[AccType, CommandType]]],
                        array: Phrase[ExpType],
                        out: Phrase[AccType])
                       (implicit context: TranslationContext): Phrase[CommandType] =
    MapParI(n, dt1, dt2, f, array, out)
}
