package idealised.OpenCL.ImperativePrimitives

import idealised.C.AST._
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{AccType, CommType, DataType, ExpType}
import idealised.DPIA._
import idealised._
import lift.arithmetic.{?, ContinuousRange, PosInf, RangeAdd}
import idealised.OpenCL._

//noinspection TypeAnnotation
final case class ParForWorkGroup(dim: Int)(override val n: Nat,
                                           override val dt: DataType,
                                           override val out: Phrase[AccType],
                                           override val body: Phrase[ExpType ->: AccType ->: CommType])
  extends OpenCLParFor(n, dt, out, body) {

  lazy val num_groups: Nat = ?
//    if (env.globalSize == ? || env.localSize == ?) ?
//    else env.globalSize /^ env.localSize

  override def makeParFor = ParForWorkGroup(dim)

  override val parallelismLevel = OpenCL.WorkGroup

  override val name: String = freshName("wg_id_")

  override lazy val init: BuiltInFunctionCall = get_group_id(dim)

  override lazy val step: BuiltInFunctionCall = get_num_groups(dim, num_groups_range)

  lazy val num_groups_range: RangeAdd =
    if (num_groups == ?) ContinuousRange(1, PosInf)
    else RangeAdd(num_groups, num_groups + 1, 1)

  override def synchronize: Stmt = Comment("par for workgroup sync")

}
