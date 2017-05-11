package idealised.OpenCL.LowLevelPrimitives

import idealised._
import idealised.Core._
import lift.arithmetic.{?, ContinuousRange, PosInf, RangeAdd}
import opencl.generator.OpenCLAST._
import opencl.generator.{get_local_id, get_local_size}


final case class ParForLocal(override val n: Nat,
                             override val dt: DataType,
                             override val out: Phrase[AccType],
                             override val body: Phrase[ExpType -> (AccType -> CommandType)])
  extends OpenCLParFor(n, dt, out, body) {

  override def makeParFor = ParForLocal

  override val parallelismLevel = OpenCL.Local

  override lazy val init = get_local_id(0, RangeAdd(0, env.localSize, 1))

  override lazy val step = get_local_size(0, local_size_range)

  lazy val local_size_range =
    if (env.localSize == ?) ContinuousRange(1, PosInf)
    else RangeAdd(env.localSize, env.localSize + 1, 1)

  override def synchronize: OclAstNode with BlockMember =
    OpenCLCode("barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);")

}
