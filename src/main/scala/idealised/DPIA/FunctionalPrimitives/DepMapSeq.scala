package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.IntermediatePrimitives.{DepMapSeqI, DepMapSeqIUnroll}
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA._

//noinspection TypeAnnotation
final case class DepMapSeq(n: Nat,
                           ft1:NatDataTypeFunction,
                           ft2: NatDataTypeFunction,
                           f: Phrase[`(nat)->`[ExpType -> ExpType]],
                           array: Phrase[ExpType])
  extends AbstractDepMap(n, ft1, ft2, f, array)
{
  override def makeMap = DepMapSeq

  override def makeMapI(n: Nat,
                        ft1:NatDataTypeFunction,
                        ft2: NatDataTypeFunction,
                        f: Phrase[`(nat)->`[->[ExpType, ->[AccType, CommandType]]]],
                        array: Phrase[ExpType],
                        out: Phrase[AccType])
                       (implicit context: TranslationContext) =
    DepMapSeqI(n, ft1, ft2, f, array, out)
}

//noinspection TypeAnnotation
final case class DepMapSeqUnroll(n: Nat,
                                 ft1:NatDataTypeFunction,
                                 ft2: NatDataTypeFunction,
                           f: Phrase[`(nat)->`[ExpType -> ExpType]],
                           array: Phrase[ExpType])
  extends AbstractDepMap(n, ft1, ft2, f, array)
{
  override def makeMap = DepMapSeqUnroll

  override def makeMapI(n: Nat,
                        ft1:NatDataTypeFunction,
                        ft2: NatDataTypeFunction,
                        f: Phrase[`(nat)->`[->[ExpType, ->[AccType, CommandType]]]],
                        array: Phrase[ExpType],
                        out: Phrase[AccType])
                       (implicit context: TranslationContext) =
    DepMapSeqIUnroll(n, ft1, ft2, f, array, out)
}