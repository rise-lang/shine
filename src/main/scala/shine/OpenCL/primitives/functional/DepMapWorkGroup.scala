package shine.OpenCL.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.DPIA.primitives.functional.AbstractDepMap
import shine.OpenCL.primitives.intermediate.DepMapWorkGroupI

final case class DepMapWorkGroup(dim: Int)(n: Nat,
                                           ft1: NatToData,
                                           ft2: NatToData,
                                           f: Phrase[`(nat)->:`[ExpType ->: ExpType]],
                                           array: Phrase[ExpType]) extends AbstractDepMap(n, ft1, ft2, f, array) {
  override def makeMap = DepMapWorkGroup(dim)

  override def makeMapI(n: Nat,
                        ft1: NatToData,
                        ft2: NatToData,
                        f: Phrase[`(nat)->:`[ExpType ->: AccType ->: CommType]],
                        array: Phrase[ExpType],
                        out: Phrase[AccType])
                       (implicit context: TranslationContext): Phrase[CommType] =
    DepMapWorkGroupI(dim)(n, ft1, ft2, f, array, out)
}
