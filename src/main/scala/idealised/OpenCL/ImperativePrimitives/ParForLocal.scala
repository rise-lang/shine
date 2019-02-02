package idealised.OpenCL.ImperativePrimitives

import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{AccType, CommandType, DataType, ExpType}
import idealised.DPIA._
import idealised._
import lift.arithmetic.{?, ContinuousRange, PosInf, RangeAdd}
import idealised.C.AST._
import idealised.OpenCL.AST._
import idealised.OpenCL._


//noinspection TypeAnnotation
final case class ParForLocal(dim: Int)(override val n: Nat,
                                       override val dt: DataType,
                                       override val out: Phrase[AccType],
                                       override val body: Phrase[ExpType -> (AccType -> CommandType)])
  extends OpenCLParFor(n, dt, out, body) {

  override def makeParFor = ParForLocal(dim)

  override val parallelismLevel = OpenCL.Local

  override val name: String = freshName("l_id_")

//  override lazy val init: OclFunction = get_local_id(dim, RangeAdd(0, env.localSize, 1))

  override lazy val init: BuiltInFunction = get_local_id(dim, RangeAdd(0, ?, 1))

  override lazy val step: BuiltInFunction = get_local_size(dim, local_size_range)

  lazy val local_size_range: RangeAdd = ContinuousRange(1, PosInf)
//    if (env.localSize == ?) ContinuousRange(1, PosInf)
//    else RangeAdd(env.localSize, env.localSize + 1, 1)

  override def synchronize: Stmt = Barrier(local = true, global = true)

}
