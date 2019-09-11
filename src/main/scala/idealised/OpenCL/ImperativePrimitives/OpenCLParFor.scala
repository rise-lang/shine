package idealised.OpenCL.ImperativePrimitives

import idealised.C.AST.Stmt
import idealised.OpenMP.ImperativePrimitives.AbstractParFor
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types._
import idealised.DPIA._

abstract class OpenCLParFor(n: Nat,
                            dt: DataType,
                            out: Phrase[AccType],
                            body: Phrase[ExpType ->: AccType ->: CommType],
                            val init: Nat,
                            val step: Nat,
                            val unroll: Boolean)
  extends AbstractParFor(n, dt, out, body) {

  def parallelismLevel: idealised.OpenCL.ParallelismLevel

//  protected var env: OpenCLOldCodeGenerator.Environment = _

  def name: String

  def synchronize: Stmt
}

object OpenCLParFor {
  def unapply(arg: OpenCLParFor): Option[(Nat, DataType, Phrase[AccType], Phrase[ExpType ->: AccType ->: CommType], Nat, Nat, Boolean)] =
    Some((arg.n, arg.dt, arg.out, arg.body, arg.init, arg.step, arg.unroll))
}
