package idealised.OpenMP.ImperativePrimitives

import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA._

//noinspection TypeAnnotation
final case class ParFor(override val n: Nat,
                        override val dt: DataType,
                        override val out: Phrase[AccType],
                        override val body: Phrase[ExpType -> (AccType -> CommandType)])
  extends AbstractParFor[DataType](n, dt, out, body) {
  override def makeParFor = ParFor
}
