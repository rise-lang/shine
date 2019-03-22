package idealised.OpenMP.FunctionalPrimitives

import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.FunctionalPrimitives.AbstractDepMap
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types._
import idealised.DPIA.{->, Nat, NatDataTypeFunction, NatIdentifier, `(nat)->`}
import idealised.OpenMP.IntermediatePrimitives.DepMapParI

//noinspection TypeAnnotation
final case class DepMapPar(n: Nat,
                           ft1:NatDataTypeFunction,
                           ft2:NatDataTypeFunction,
                           f: Phrase[`(nat)->`[ExpType -> ExpType]],
                           array: Phrase[ExpType])
  extends AbstractDepMap(n, ft1, ft2, f, array)
{
  override def makeMap = DepMapPar


  override def makeMapI(n: Nat,
                        ft1:NatDataTypeFunction,
                        ft2:NatDataTypeFunction,
                        f: Phrase[`(nat)->`[->[ExpType, ->[AccType, CommandType]]]],
                        array: Phrase[ExpType],
                        out: Phrase[AccType])
                       (implicit context: TranslationContext): Phrase[CommandType] =
    DepMapParI(n, ft1, ft2, f, array, out)
}
