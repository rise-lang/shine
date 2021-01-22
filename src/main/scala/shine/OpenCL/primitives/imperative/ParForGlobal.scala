package shine.OpenCL.primitives.imperative

import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.{AccType, CommType, DataType, ExpType}
import shine.DPIA._
import shine.OpenCL
import shine.OpenCL._


//noinspection TypeAnnotation,ConvertibleToMethodValue
final case class ParForGlobal(dim: Int)(override val n: Nat,
                                        override val dt: DataType,
                                        override val out: Phrase[AccType],
                                        override val body: Phrase[ExpType ->: AccType ->: CommType],
                                        init: Nat = get_global_id(dim),
                                        step: Nat = get_global_size(dim),
                                        unroll: Boolean = false)
  extends OpenCLParFor(n, dt, out, body, init, step, unroll) {

  override val makeCLParFor =
    (n: Nat, dt: DataType, out: Phrase[AccType], body: Phrase[ExpType ->: AccType ->: CommType], init: Nat, step: Nat) =>
      ParForGlobal(dim)(n, dt, out, body, init, step, unroll)

  override val parallelismLevel = OpenCL.Global

  override val name: String = freshName("gl_id_")
}
