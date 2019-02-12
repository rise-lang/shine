package idealised.OpenCL.FunctionalPrimitives

import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.FunctionalPrimitives.AbstractDepMap
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenCL.IntermediatePrimitives.DepMapGlobalI

final case class DepMapGlobal(dim:Int)(n: Nat,
                           i1: NatIdentifier, dt1: DataType,
                           i2: NatIdentifier, dt2: DataType,
                           f: Phrase[`(nat)->`[ExpType -> ExpType]],
                           array: Phrase[ExpType])
  extends AbstractDepMap(n, i1, dt1, i2, dt2, f, array)
{
  override def makeMap = DepMapGlobal(dim)

  override def makeMapI(n: Nat,
                        i1: NatIdentifier, dt1: DataType,
                        i2: NatIdentifier, dt2: DataType,
                        f: Phrase[`(nat)->`[->[ExpType, ->[AccType, CommandType]]]],
                        array: Phrase[ExpType],
                        out: Phrase[AccType])
                       (implicit context: TranslationContext): Phrase[CommandType] =
    DepMapGlobalI(dim)(n, i2, dt1, i2, dt2, f, array, out)
}
