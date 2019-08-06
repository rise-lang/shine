package idealised.OpenCL.ImperativePrimitives

import idealised.C.AST.Stmt
import idealised.OpenMP.ImperativePrimitives.AbstractParFor
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types._
import idealised.DPIA._

abstract class OpenCLParFor(n: Nat,
                            dt: DataType,
                            out: Phrase[AccType],
                            body: Phrase[ExpType ->: AccType ->: CommType])
  extends AbstractParFor(n, dt, out, body) {

  def parallelismLevel: idealised.OpenCL.ParallelismLevel

//  protected var env: OpenCLOldCodeGenerator.Environment = _

  def name: String

  def init: Nat
  def step: Nat
  def synchronize: Stmt
}

object OpenCLParFor {
  def unapply(arg: OpenCLParFor): Option[(Nat, DataType, Phrase[AccType], Phrase[ExpType ->: AccType ->: CommType])] =
    Some((arg.n, arg.dt, arg.out, arg.body))
}
