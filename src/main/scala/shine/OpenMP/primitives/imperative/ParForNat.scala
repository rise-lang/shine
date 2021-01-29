package shine.OpenMP.primitives.imperative

import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types._
import shine.DPIA._

//noinspection TypeAnnotation
final case class ParForNat(override val n: Nat,
                           override val ft: NatToData,
                           override val out: Phrase[AccType],
                           override val body: Phrase[`(nat)->:`[AccType ->: CommType]])
  extends AbstractParForNat(n, ft, out, body) {
  override def makeParForNat = ParForNat
}
