package idealised.OpenCL.FunctionalPrimitives

/**
  * Created by federico on 13/01/18.
  */
import idealised.DPIA.FunctionalPrimitives.AbstractScan
import idealised.DPIA.IntermediatePrimitives.ScanSeqI
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{DataType, ExpType}
import idealised.DPIA._

final case class ScanSeq(n: Nat,
                           dt1: DataType, dt2: DataType,
                           f: Phrase[ExpType -> (ExpType -> ExpType)],
                           init: Phrase[ExpType],
                           array: Phrase[ExpType])
  extends AbstractScan(n, dt1, dt2, f, init, array) {
  override def makeScan = ScanSeq

  override def makeScanI = ScanSeqI
}
