package shine.cuda.primitives.imperative

import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.{AccType, CommType, DataType, ExpType}
import shine.DPIA._
import shine.OpenCL
import shine.cuda._

//noinspection TypeAnnotation,ConvertibleToMethodValue
final case class ParForThreads(dim: Char)(
  override val n: Nat,
  override val dt: DataType,
  override val out: Phrase[AccType],
  override val body: Phrase[ExpType ->: AccType ->: CommType],
  init: Nat = threadId(dim),
  step: Nat = blockDim(dim),
  unroll: Boolean = false
) extends CudaParFor(n, dt, out, body, init, step, unroll) {

  override val makeCLParFor =
    (n: Nat, dt: DataType, out: Phrase[AccType],
     body: Phrase[ExpType ->: AccType ->: CommType], init: Nat, step: Nat) =>
      ParForThreads(dim)(n, dt, out, body, init, step, unroll)

  override val parallelismLevel = OpenCL.Local

  override val name: String = freshName("tid_")
}
