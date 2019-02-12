package idealised.OpenMP.FunctionalPrimitives

import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.FunctionalPrimitives.AbstractDepMap
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types._
import idealised.DPIA.{->, Nat, NatIdentifier, `(nat)->`}
import idealised.OpenMP.IntermediatePrimitives.DepMapParI

//noinspection TypeAnnotation
final case class DepMapPar(n: Nat,
                           i1: NatIdentifier, dt1: DataType,
                           i2: NatIdentifier, dt2: DataType,
                           f: Phrase[`(nat)->`[ExpType -> ExpType]],
                           array: Phrase[ExpType])
  extends AbstractDepMap(n, i1, dt1, i2, dt2, f, array)
{
  override def makeMap = DepMapPar


  override def makeMapI(n: Nat,
                        i1: NatIdentifier, dt1: DataType,
                        i2: NatIdentifier, dt2: DataType,
                        f: Phrase[`(nat)->`[->[ExpType, ->[AccType, CommandType]]]],
                        array: Phrase[ExpType],
                        out: Phrase[AccType])
                       (implicit context: TranslationContext): Phrase[CommandType] =
    DepMapParI(n, i2, dt1, i2, dt2, f, array, out)
}
