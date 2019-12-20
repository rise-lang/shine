package shine.OpenCL.ImperativePrimitives

import shine.C.AST.{Comment, Stmt}
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.{AccType, CommType, NatToData}
import shine.DPIA.{->:, Nat, `(nat)->:`, freshName}
import shine.OpenCL
import shine.OpenCL.{get_group_id, get_num_groups}

final case class ParForNatWorkGroup(dim:Int)(override val n:Nat,
                                             override val ft:NatToData,
                                             override val out:Phrase[AccType],
                                             override val body: Phrase[`(nat)->:`[AccType ->: CommType]],
                                             init: Nat = get_group_id(dim),
                                             step: Nat = get_num_groups(dim),
                                             unroll: Boolean = false)
  extends OpenCLParForNat(n, ft, out, body, init, step, unroll) {

  def makeParForNat =
    (n, ft, out, body) => ParForNatWorkGroup(dim)(n, ft, out, body, init, step, unroll)

  override val parallelismLevel = OpenCL.WorkGroup

  override val name: String = freshName("wg_id_")
}

