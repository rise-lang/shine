package idealised.OpenCL.ImperativePrimitives

import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{AccType, CommType, DataType, ExpType}
import idealised.DPIA._
import idealised._
import lift.arithmetic.{?, ContinuousRange, PosInf, RangeAdd}
import idealised.C.AST._
import idealised.OpenCL.AST._
import idealised.OpenCL._


//noinspection TypeAnnotation
final case class ParForLocal(dim: Int)(override val n: Nat,
                                       override val dt: DataType,
                                       override val out: Phrase[AccType],
                                       override val body: Phrase[ExpType ->: AccType ->: CommType],
                                       init: Nat = get_local_id(dim),
                                       step: Nat = get_local_size(dim),
                                       unroll: Boolean = false)
  extends OpenCLParFor(n, dt, out, body, init, step, unroll) {

  override val makeCLParFor =
    (n: Nat, dt: DataType, out: Phrase[AccType], body: Phrase[ExpType ->: AccType ->: CommType], init: Nat, step: Nat) =>
      ParForLocal(dim)(n, dt, out, body, init, step, unroll)

  override val parallelismLevel = OpenCL.Local

  override val name: String = freshName("l_id_")
}
