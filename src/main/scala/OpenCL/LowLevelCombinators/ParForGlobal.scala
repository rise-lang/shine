package OpenCL.LowLevelCombinators

import Core._
import LowLevelCombinators.AbstractParFor
import apart.arithmetic.{ArithExpr, RangeAdd}
import opencl.generator.OpenCLAST._
import opencl.generator.{get_global_id, get_global_size}


final case class ParForGlobal(n: ArithExpr,
                              dt: DataType,
                              out: Phrase[AccType],
                              body: Phrase[ExpType -> (AccType -> CommandType)])
  extends OpenCLParFor(n, dt, out, body) {

  override val makeParFor = ParForGlobal

  override lazy val init = get_global_id(0, RangeAdd(0, env.globalSize, 1))

  override lazy val step = get_global_size(0, RangeAdd(env.globalSize, env.globalSize + 1, 1))

  override def synchronize: OclAstNode with BlockMember = Comment("par for global sync")
}
