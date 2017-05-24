package idealised.OpenCL.ImperativePrimitives

import idealised._
import idealised.utils._
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{AccType, CommandType, DataType, ExpType}
import idealised.DPIA._
import lift.arithmetic.RangeAdd
import opencl.generator.OpenCLAST._
import opencl.generator.{OclFunction, get_global_id, get_global_size}


final case class ParForGlobal(override val n: Nat,
                              override val dt: DataType,
                              override val out: Phrase[AccType],
                              override val body: Phrase[ExpType -> (AccType -> CommandType)])
  extends OpenCLParFor(n, dt, out, body) {

  override val makeParFor = ParForGlobal

  override val parallelismLevel = OpenCL.Global

  override lazy val init: OclFunction = get_global_id(0, RangeAdd(0, env.globalSize, 1))

  override lazy val step: OclFunction = get_global_size(0, RangeAdd(env.globalSize, env.globalSize + 1, 1))

  override def synchronize: OclAstNode with BlockMember = Comment("par for global sync")
}
