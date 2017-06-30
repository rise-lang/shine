package idealised.OpenCL.ImperativePrimitives

import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{AccType, CommandType, DataType, ExpType}
import idealised.DPIA._
import idealised._
import lift.arithmetic.RangeAdd
import opencl.generator.OpenCLAST._
import opencl.generator.{OclFunction, get_global_id, get_global_size}


final case class ParForGlobal(dim: Int)(override val n: Nat,
                                        override val dt: DataType,
                                        override val out: Phrase[AccType],
                                        override val body: Phrase[ExpType -> (AccType -> CommandType)])
  extends OpenCLParFor(n, dt, out, body) {

  override val makeParFor = ParForGlobal(dim) _

  override val parallelismLevel = OpenCL.Global

  override val name: String = freshName("gl_id_")

  override lazy val init: OclFunction = get_global_id(dim, RangeAdd(0, env.globalSize, 1))

  override lazy val step: OclFunction = get_global_size(dim, RangeAdd(env.globalSize, env.globalSize + 1, 1))

  override def synchronize: OclAstNode with BlockMember = Comment("par for global sync")
}
