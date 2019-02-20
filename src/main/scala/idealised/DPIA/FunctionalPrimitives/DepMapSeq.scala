package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.Compilation.TranslationContext
import idealised.DPIA.IntermediatePrimitives.{DepMapSeqI, DepMapSeqIUnroll}
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA._

//noinspection TypeAnnotation
final case class DepMapSeq(n: Nat,
                           i1: NatIdentifier, dt1: DataType,
                           i2: NatIdentifier, dt2: DataType,
                           f: Phrase[`(nat)->`[ExpType -> ExpType]],
                           array: Phrase[ExpType])
  extends AbstractDepMap(n, i1, dt1, i2, dt2, f, array)
{
  override def makeMap = DepMapSeq

  override def makeMapI(n: Nat,
                        i1: NatIdentifier, dt1: DataType,
                        i2: NatIdentifier, dt2: DataType,
                        f: Phrase[`(nat)->`[->[ExpType, ->[AccType, CommandType]]]],
                        array: Phrase[ExpType],
                        out: Phrase[AccType])
                       (implicit context: TranslationContext) =
    DepMapSeqI(n, i1, dt1, i2, dt2, f, array, out)
}

//noinspection TypeAnnotation
final case class DepMapSeqUnroll(n: Nat,
                           i1: NatIdentifier, dt1: DataType,
                           i2: NatIdentifier, dt2: DataType,
                           f: Phrase[`(nat)->`[ExpType -> ExpType]],
                           array: Phrase[ExpType])
  extends AbstractDepMap(n, i1, dt1, i2, dt2, f, array)
{
  override def makeMap = DepMapSeqUnroll

  override def makeMapI(n: Nat,
                        i1: NatIdentifier, dt1: DataType,
                        i2: NatIdentifier, dt2: DataType,
                        f: Phrase[`(nat)->`[->[ExpType, ->[AccType, CommandType]]]],
                        array: Phrase[ExpType],
                        out: Phrase[AccType])
                       (implicit context: TranslationContext) =
    DepMapSeqIUnroll(n, i1, dt1, i2, dt2, f, array, out)
}