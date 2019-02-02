package idealised.DPIA.FunctionalPrimitives

import idealised.DPIA.IntermediatePrimitives.DepMapSeqI
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA._

//noinspection TypeAnnotation
final case class DepMapSeq(n: Nat,
                           i1: NatIdentifier, dt1: DataType,
                           i2: NatIdentifier, dt2: DataType,
                           f: Phrase[`(nat)->`[ExpType -> ExpType]],
                           array: Phrase[ExpType])
  extends AbstractDepMap(n, i1, dt1, i2, dt2, f, array) {
  override def makeMap = DepMapSeq

  override def makeMapI = DepMapSeqI
}