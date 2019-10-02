package idealised.OpenCL.ImperativePrimitives

import idealised.C.AST._
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{AccType, CommType, DataType, ExpType}
import idealised.DPIA._
import idealised.OpenCL
import idealised.OpenCL._


//noinspection TypeAnnotation,ConvertibleToMethodValue
final case class ParForGlobal(dim: Int)(override val n: Nat,
                                        override val dt: DataType,
                                        override val out: Phrase[AccType],
                                        override val body: Phrase[ExpType ->: AccType ->: CommType],
                                        init: Nat = get_global_id(dim),
                                        step: Nat = get_global_size(dim),
                                        unroll: Boolean = false)
  extends OpenCLParFor(n, dt, out, body, init, step, unroll) {

  override val makeCLParFor =
    (n: Nat, dt: DataType, out: Phrase[AccType], body: Phrase[ExpType ->: AccType ->: CommType], init: Nat, step: Nat) =>
      ParForGlobal(dim)(n, dt, out, body, init, step, unroll)

  override val parallelismLevel = OpenCL.Global

  override val name: String = freshName("gl_id_")

  override def synchronize: Stmt = Comment("par for global sync")
}
