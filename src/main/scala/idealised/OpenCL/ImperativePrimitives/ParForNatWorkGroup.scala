package idealised.OpenCL.ImperativePrimitives

import idealised.C.AST.{Comment, Stmt}
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{AccType, CommType, NatToData}
import idealised.DPIA.{->:, Nat, `(nat)->:`, freshName}
import idealised.OpenCL
import idealised.OpenCL.{BuiltInFunctionCall, get_group_id, get_num_groups}
import lift.arithmetic.{?, ContinuousRange, PosInf, RangeAdd}

final case class ParForNatWorkGroup(dim:Int)(override val n:Nat,
                                             override val ft:NatToData,

                                             override val out:Phrase[AccType],
                                             override val body: Phrase[`(nat)->:`[AccType ->: CommType]])
  extends OpenCLParForNat(n, ft, out, body) {
  override val makeParForNat = ParForNatWorkGroup(dim) _

  override val parallelismLevel = OpenCL.WorkGroup

  lazy val num_groups: Nat = ?
  //    if (env.globalSize == ? || env.localSize == ?) ?
  //    else env.globalSize /^ env.localSize

  override val name: String = freshName("wg_id_")

  override lazy val init: BuiltInFunctionCall = get_group_id(dim)

  override lazy val step: BuiltInFunctionCall = get_num_groups(dim, num_groups_range)

  lazy val num_groups_range: RangeAdd =
    if (num_groups == ?) ContinuousRange(1, PosInf)
    else RangeAdd(num_groups, num_groups + 1, 1)

  override def synchronize: Stmt = Comment("par for workgroup sync")
}

