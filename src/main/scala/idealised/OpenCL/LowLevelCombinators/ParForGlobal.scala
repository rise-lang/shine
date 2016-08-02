package idealised.OpenCL.LowLevelCombinators

import idealised._
import idealised.Core._
import apart.arithmetic.RangeAdd
import opencl.generator.OpenCLAST._
import opencl.generator.{get_global_id, get_global_size}


final case class ParForGlobal(override val n: Nat,
                              override val dt: DataType,
                              override val out: Phrase[AccType],
                              override val body: Phrase[ExpType -> (AccType -> CommandType)])
  extends OpenCLParFor(n, dt, out, body) {

  override val makeParFor = ParForGlobal

  override val parallelismLevel = OpenCL.Global

  override lazy val init = get_global_id(0, RangeAdd(0, env.globalSize, 1))

  override lazy val step = get_global_size(0, RangeAdd(env.globalSize, env.globalSize + 1, 1))

  override def synchronize: OclAstNode with BlockMember = Comment("par for global sync")
}
