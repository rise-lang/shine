package idealised.OpenCL.ImperativePrimitives

import idealised.C.AST.Stmt
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{AccType, CommType, NatToData}
import idealised.DPIA.{->:, Nat, `(nat)->:`, freshName}
import idealised.OpenCL
import idealised.OpenCL.{get_local_id, get_local_size}

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

  override def synchronize: Stmt = idealised.OpenCL.AST.Barrier(local = true, global = true)
}
