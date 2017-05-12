package idealised.OpenCL.ImperativePrimitives

import idealised._
import idealised.Core._
import lift.arithmetic.{?, ContinuousRange, PosInf, RangeAdd}
import opencl.generator.OpenCLAST._
import opencl.generator.{OclFunction, get_group_id, get_num_groups}

final case class ParForWorkGroup(override val n: Nat,
                                 override val dt: DataType,
                                 override val out: Phrase[AccType],
                                 override val body: Phrase[ExpType -> (AccType -> CommandType)])
  extends OpenCLParFor(n, dt, out, body) {

  lazy val num_groups: Nat =
    if (env.globalSize == ? || env.localSize == ?) ?
    else env.globalSize /^ env.localSize

  override def makeParFor = ParForWorkGroup

  override val parallelismLevel = OpenCL.WorkGroup

  override lazy val init: OclFunction = get_group_id(0, RangeAdd(0, num_groups, 1))

  override lazy val step: OclFunction = get_num_groups(0, num_groups_range)

  lazy val num_groups_range: RangeAdd =
    if (num_groups == ?) ContinuousRange(1, PosInf)
    else RangeAdd(num_groups, num_groups + 1, 1)

  override def synchronize: OclAstNode with BlockMember = Comment("par for workgroup sync")

}
