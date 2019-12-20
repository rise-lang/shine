package shine.OpenCL.ImperativePrimitives

import shine.C.AST.Stmt
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.{AccType, CommType, NatToData}
import shine.DPIA.{->:, Nat, `(nat)->:`, freshName}
import shine.OpenCL
import shine.OpenCL.{get_local_id, get_local_size}

final case class ParForNatLocal(dim:Int)(override val n:Nat,
                                         override val ft:NatToData,
                                         override val out:Phrase[AccType],
                                         override val body: Phrase[`(nat)->:`[AccType ->: CommType]],
                                         init: Nat = get_local_id(dim),
                                         step: Nat = get_local_size(dim),
                                         unroll: Boolean = false)
  extends OpenCLParForNat(n, ft, out, body, init, step, unroll) {

  def makeParForNat =
    (n, ft, out, body) => ParForNatLocal(dim)(n, ft, out, body, init, step, unroll)

  override val parallelismLevel = OpenCL.Local

  override val name: String = freshName("l_id_")
}
