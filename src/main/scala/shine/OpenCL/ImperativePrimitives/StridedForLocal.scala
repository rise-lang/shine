package shine.OpenCL.ImperativePrimitives

import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.{CommType, ExpType}
import shine.DPIA._
import shine.OpenCL._
import shine._


//noinspection TypeAnnotation
final case class StridedForLocal(dim: Int)(override val n: Nat,
                                           override val body: Phrase[ExpType ->: CommType],
                                           init: Nat = get_local_id(dim),
                                           step: Nat = get_local_size(dim),
                                           unroll: Boolean = false)
  extends OpenCLStridedFor(n, body, init, step, unroll) {

  override val makeCLStridedFor =
    (n: Nat, body: Phrase[ExpType ->: CommType], init: Nat, step: Nat, unroll: Boolean) =>
      StridedForLocal(dim)(n, body, init, step, unroll)

  override val parallelismLevel = OpenCL.Local

  override val name: String = freshName("l_id_")
}
