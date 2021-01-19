package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.primitives.intermediate.{DepMapSeqI, DepMapSeqIUnroll}
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._

//noinspection TypeAnnotation
final case class DepMapSeq(n: Nat,
                           ft1:NatToData,
                           ft2: NatToData,
                           f: Phrase[`(nat)->:`[ExpType ->: ExpType]],
                           array: Phrase[ExpType])
  extends AbstractDepMap(n, ft1, ft2, f, array)
{
  override def makeMap = DepMapSeq

  override def makeMapI(n: Nat,
                        ft1:NatToData,
                        ft2: NatToData,
                        f: Phrase[`(nat)->:`[ExpType ->: AccType ->: CommType]],
                        array: Phrase[ExpType],
                        out: Phrase[AccType])
                       (implicit context: TranslationContext) =
    DepMapSeqI(n, ft1, ft2, f, array, out)
}

//noinspection TypeAnnotation
final case class DepMapSeqUnroll(n: Nat,
                                 ft1:NatToData,
                                 ft2: NatToData,
                                 f: Phrase[`(nat)->:`[ExpType ->: ExpType]],
                                 array: Phrase[ExpType])
  extends AbstractDepMap(n, ft1, ft2, f, array)
{
  override def makeMap = DepMapSeqUnroll

  override def makeMapI(n: Nat,
                        ft1:NatToData,
                        ft2: NatToData,
                        f: Phrase[`(nat)->:`[ExpType ->: AccType ->: CommType]],
                        array: Phrase[ExpType],
                        out: Phrase[AccType])
                       (implicit context: TranslationContext) =
    DepMapSeqIUnroll(n, ft1, ft2, f, array, out)
}