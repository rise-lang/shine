package shine.OpenCL.FunctionalPrimitives

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.FunctionalPrimitives.AbstractDepMap
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL.IntermediatePrimitives.DepMapGlobalI

final case class DepMapGlobal(dim:Int)(val n: Nat,
                                       val ft1:NatToData,
                                       val ft2:NatToData,
                                       val f: Phrase[`(nat)->:`[ExpType ->: ExpType]],
                                       val array: Phrase[ExpType])
  extends AbstractDepMap(n, ft1, ft2, f, array)
{
  override def makeMap = DepMapGlobal(dim)

  override def makeMapI(n: Nat,
                        ft1:NatToData,
                        ft2:NatToData,
                        f: Phrase[`(nat)->:`[ExpType ->: AccType ->:CommType]],
                        array: Phrase[ExpType],
                        out: Phrase[AccType])
                       (implicit context: TranslationContext): Phrase[CommType] =
    DepMapGlobalI(dim)(n, ft1, ft2, f, array, out)
}
