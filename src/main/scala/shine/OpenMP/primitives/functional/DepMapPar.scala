package shine.OpenMP.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types._
import shine.DPIA.primitives.functional.AbstractDepMap
import shine.DPIA.{->:, Nat, `(nat)->:`}
import shine.OpenMP.primitives.intermediate

//noinspection TypeAnnotation
final case class DepMapPar(n: Nat,
                           ft1: NatToData,
                           ft2: NatToData,
                           f: Phrase[`(nat)->:`[ExpType ->: ExpType]],
                           array: Phrase[ExpType])
  extends AbstractDepMap(n, ft1, ft2, f, array) {
  override def makeMap = DepMapPar


  override def makeMapI(n: Nat,
                        ft1: NatToData,
                        ft2: NatToData,
                        f: Phrase[`(nat)->:`[ExpType ->: AccType ->: CommType]],
                        array: Phrase[ExpType],
                        out: Phrase[AccType])
                       (implicit context: TranslationContext): Phrase[CommType] =
    intermediate.DepMapParI(n, ft1, ft2, f, array, out)
}
