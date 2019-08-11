package idealised.OpenCL.ImperativePrimitives

import idealised.C.AST.{Comment, Stmt}
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{AccType, CommType, NatToData}
import idealised.DPIA.{->:, Nat, `(nat)->:`, freshName}
import idealised.OpenCL
import idealised.OpenCL._
import lift.arithmetic.{?, RangeAdd}

//noinspection TypeAnnotation,ConvertibleToMethodValue
final case class ParForNatGlobal(dim:Int)(override val n:Nat,
                                          override val ft:NatToData,
                                          override val out:Phrase[AccType],
                                          override val body: Phrase[`(nat)->:`[AccType ->: CommType]])
  extends OpenCLParForNat(n, ft, out, body) {
  override val makeParForNat = ParForNatGlobal(dim) _

  override val parallelismLevel: OpenCL.Global.type = OpenCL.Global

  override val name: String = freshName("gl_id_")

  //  override lazy val init: OclFunction = get_global_id(dim, RangeAdd(0, env.globalSize, 1))
  //
  //  override lazy val step: OclFunction = get_global_size(dim, RangeAdd(env.globalSize, env.globalSize + 1, 1))

  override lazy val init: BuiltInFunction = get_global_id(dim)

  override lazy val step: BuiltInFunction = get_global_size(dim, RangeAdd(?, ? + 1, 1))

  override def synchronize: Stmt = Comment("par for global sync")
}