package shine.OpenMP.primitives.imperative

import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.{AccType, CommType, DataType, ExpType}
import shine.DPIA.{->:, Nat}

//noinspection TypeAnnotation
final case class ParFor(override val n: Nat,
                        override val dt: DataType,
                        override val out: Phrase[AccType],
                        override val body: Phrase[ExpType ->: AccType ->: CommType])
  extends AbstractParFor[DataType](n, dt, out, body) {
  override def makeParFor = ParFor
}
