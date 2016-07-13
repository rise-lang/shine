package OpenCL.LowLevelCombinators

import Core._
import apart.arithmetic.{?, ContinuousRange, PosInf, RangeAdd}
import opencl.generator.OpenCLAST._
import opencl.generator.{get_group_id, get_num_groups}

final case class ParForWorkGroup(n: Nat,
                                 dt: DataType,
                                 out: Phrase[AccType],
                                 body: Phrase[ExpType -> (AccType -> CommandType)])
  extends OpenCLParFor(n, dt, out, body) {

  lazy val num_groups =
    if (env.globalSize == ? || env.localSize == ?) ?
    else env.globalSize /^ env.localSize

  override def makeParFor = ParForWorkGroup

  override lazy val init = get_group_id(0, RangeAdd(0, num_groups, 1))

  override lazy val step = get_num_groups(0, num_groups_range)

  lazy val num_groups_range =
    if (num_groups == ?) ContinuousRange(1, PosInf)
    else RangeAdd(num_groups, num_groups + 1, 1)

  override def synchronize: OclAstNode with BlockMember = Comment("par for workgroup sync")

}
