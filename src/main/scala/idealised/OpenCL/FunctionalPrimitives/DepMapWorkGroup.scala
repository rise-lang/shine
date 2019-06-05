package idealised.OpenCL.FunctionalPrimitives

import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.FunctionalPrimitives.AbstractDepMap
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenCL.IntermediatePrimitives.DepMapWorkGroupI

final case class DepMapWorkGroup(dim: Int)(n: Nat,
                                           ft1: NatDataTypeFunction,
                                           ft2: NatDataTypeFunction,
                                           f: Phrase[`(nat)->`[ExpType -> ExpType]],
                                           array: Phrase[ExpType]) extends AbstractDepMap(n, ft1, ft2, f, array) {
  override def makeMap = DepMapWorkGroup(dim)

  override def makeMapI(n: Nat,
                        ft1: NatDataTypeFunction,
                        ft2: NatDataTypeFunction,
                        f: Phrase[`(nat)->`[->[ExpType, ->[AccType, CommType]]]],
                        array: Phrase[ExpType],
                        out: Phrase[AccType])
                       (implicit context: TranslationContext): Phrase[CommType] =
    DepMapWorkGroupI(dim)(n, ft1, ft2, f, array, out)
}
