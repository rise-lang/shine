package idealised.OpenCL.ImperativePrimitives

import idealised.C.AST.{Comment, Stmt}
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{AccType, CommType, NatToData}
import idealised.DPIA.{->:, Nat, `(nat)->:`, freshName}
import idealised.OpenCL
import idealised.OpenCL._
import arithexpr.arithmetic.{?, RangeAdd}

//noinspection TypeAnnotation,ConvertibleToMethodValue
final case class ParForNatGlobal(dim:Int)(override val n:Nat,
                                          override val ft:NatToData,
                                          override val out:Phrase[AccType],
                                          override val body: Phrase[`(nat)->:`[AccType ->: CommType]],
                                          init: Nat = get_global_id(dim),
                                          step: Nat = get_global_size(dim),
                                          unroll: Boolean = false)
  extends OpenCLParForNat(n, ft, out, body, init, step, unroll) {

  def makeParForNat =
    (n, ft, out, body) => ParForNatGlobal(dim)(n, ft, out, body, init, step, unroll)

  override val parallelismLevel: OpenCL.Global.type = OpenCL.Global

  override val name: String = freshName("gl_id_")
}