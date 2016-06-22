package CommandPatterns

import Core.PhraseType._
import Core._
import apart.arithmetic._
import opencl.generator.OpenCLAST._
import opencl.generator.{get_local_id, get_local_size}


case class ParForLocal(override val n: ArithExpr,
                       override val dt: DataType,
                       override val out: Phrase[AccType],
                       override val body: Phrase[ExpType -> (AccType -> CommandType)])
  extends AbstractParFor(n, dt, out, body) {

  override def makeParFor = ParForLocal

  override lazy val init =  get_local_id(0, RangeAdd(0, ocl.localSize, 1))

  override lazy val step = get_local_size(0, local_size_range)

  lazy val local_size_range =
    if (ocl.localSize == ?) ContinuousRange(1, PosInf)
    else RangeAdd(ocl.localSize, ocl.localSize + 1, 1)

  override def synchronize: OclAstNode with BlockMember =
    OpenCLCode("barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);")

}
