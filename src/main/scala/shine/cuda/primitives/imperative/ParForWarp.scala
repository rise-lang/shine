package shine.cuda.primitives.imperative

import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.{AccType, CommType, DataType, ExpType}
import shine.DPIA._
import shine.OpenCL
import shine.cuda._

//noinspection TypeAnnotation,ConvertibleToMethodValue
final case class ParForWarp(dim: Char)(
  override val n: Nat,
  override val dt: DataType,
  override val out: Phrase[AccType],
  override val body: Phrase[ExpType ->: AccType ->: CommType],
  init: Nat = warpId(dim),
  step: Nat = warpDim(dim),
  unroll: Boolean = false
) extends CudaParFor(n, dt, out, body, init, step, unroll) {

  override val name: String = freshName("warp_id_")

  override val makeCLParFor =
    (n: Nat, dt: DataType, out: Phrase[AccType],
     body: Phrase[ExpType ->: AccType ->: CommType], init: Nat, step: Nat) =>
      ParForWarp(dim)(n, dt, out, body, init, step, unroll)

  override val parallelismLevel = OpenCL.Local
}
