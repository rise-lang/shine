package idealised.OpenCL.ImperativePrimitives

import idealised.C.AST.Stmt
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{AccType, CommType, NatToData}
import idealised.DPIA.{->:, Nat, `(nat)->:`, freshName}
import idealised.OpenCL
import idealised.OpenCL.AST.Barrier
import idealised.OpenCL.{BuiltInFunctionCall, get_local_id, get_local_size}
import lift.arithmetic.{?, ContinuousRange, PosInf, RangeAdd}

final case class ParForNatLocal(dim:Int)(override val n:Nat,
                                         override val ft:NatToData,
                                         override val out:Phrase[AccType],
                                         override val body: Phrase[`(nat)->:`[AccType ->: CommType]])
  extends OpenCLParForNat(n, ft, out, body) {
  override val makeParForNat = ParForNatLocal(dim) _

  override val parallelismLevel = OpenCL.Local

  override val name: String = freshName("l_id_")

  //  override lazy val init: OclFunction = get_local_id(dim, RangeAdd(0, env.localSize, 1))

  override lazy val init: BuiltInFunctionCall = get_local_id(dim)

  override lazy val step: BuiltInFunctionCall = get_local_size(dim, local_size_range)

  lazy val local_size_range: RangeAdd = ContinuousRange(1, PosInf)
  //    if (env.localSize == ?) ContinuousRange(1, PosInf)
  //    else RangeAdd(env.localSize, env.localSize + 1, 1)

  override def synchronize: Stmt = Barrier(local = true, global = true)
}
