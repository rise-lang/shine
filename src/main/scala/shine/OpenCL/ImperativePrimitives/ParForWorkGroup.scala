package shine.OpenCL.ImperativePrimitives

import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.{AccType, CommType, DataType, ExpType}
import shine.DPIA._
import shine.OpenCL._
import shine._

//noinspection TypeAnnotation
final case class ParForWorkGroup(dim: Int)(override val n: Nat,
                                           override val dt: DataType,
                                           override val out: Phrase[AccType],
                                           override val body: Phrase[ExpType ->: AccType ->: CommType],
                                           init: Nat = get_group_id(dim),
                                           step: Nat = get_num_groups(dim),
                                           unroll: Boolean = false)
  extends OpenCLParFor(n, dt, out, body, init, step, unroll) {

  override val makeCLParFor =
    (n: Nat, dt: DataType, out: Phrase[AccType], body: Phrase[ExpType ->: AccType ->: CommType], init: Nat, step: Nat) =>
      ParForWorkGroup(dim)(n, dt, out, body, init, step, unroll)

  override val parallelismLevel = OpenCL.WorkGroup

  override val name: String = freshName("wg_id_")
}
