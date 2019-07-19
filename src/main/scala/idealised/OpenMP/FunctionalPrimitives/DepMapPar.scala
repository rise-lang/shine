package idealised.OpenMP.FunctionalPrimitives

import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.FunctionalPrimitives.AbstractDepMap
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types._
import idealised.DPIA.{->:, Nat, `(nat)->`}
import idealised.OpenMP.IntermediatePrimitives.DepMapParI

//noinspection TypeAnnotation
final case class DepMapPar(n: Nat,
                           ft1:NatToData,
                           ft2:NatToData,
                           f: Phrase[`(nat)->`[ExpType ->: ExpType]],
                           array: Phrase[ExpType])
  extends AbstractDepMap(n, ft1, ft2, f, array)
{
  override def makeMap = DepMapPar


  override def makeMapI(n: Nat,
                        ft1:NatToData,
                        ft2:NatToData,
                        f: Phrase[`(nat)->`[ExpType ->: AccType ->: CommType]],
                        array: Phrase[ExpType],
                        out: Phrase[AccType])
                       (implicit context: TranslationContext): Phrase[CommType] =
    DepMapParI(n, ft1, ft2, f, array, out)
}
