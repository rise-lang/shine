package idealised.OpenMP.ImperativePrimitives

import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA._

//noinspection TypeAnnotation
final case class ParForNat(override val n: Nat,
                           override val i: NatIdentifier,
                           override val dt: DataType,
                           override val out: Phrase[AccType],
                           override val body: Phrase[`(nat)->`[AccType -> CommandType]])
  extends AbstractParForNat[DataType](n, i, dt, out, body) {
  override def makeParForNat = ParForNat
}
