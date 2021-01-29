package shine.DPIA.primitives.functional

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.primitives.intermediate.ScanSeqI
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._

//noinspection TypeAnnotation
final case class ScanSeq(n: Nat,
                         dt1: DataType,
                         dt2: DataType,
                         f: Phrase[ExpType ->: ExpType ->: ExpType],
                         init: Phrase[ExpType],
                         array: Phrase[ExpType])
  extends AbstractScan(n, dt1, dt2, f, init, array)
{
  override def makeScan = ScanSeq

  override def makeScanI(n: Nat,
                         dt1: DataType,
                         dt2: DataType,
                         f: Phrase[->:[ExpType, ->:[ExpType, ->:[AccType, CommType]]]],
                         init: Phrase[ExpType],
                         array: Phrase[ExpType],
                         out: Phrase[AccType])
                        (implicit context: TranslationContext): Phrase[CommType] =
    ScanSeqI(n, dt1, dt2, f, init, array, out)
}
