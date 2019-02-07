package idealised.OpenCL.ImperativePrimitives

import idealised.C.AST.Stmt
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{AccType, CommandType, DataType}
import idealised.DPIA.{->, Nat, NatIdentifier, `(nat)->`}
import idealised.OpenMP.ImperativePrimitives.AbstractParForNat

abstract class OpenCLParForNat(n: Nat,
                               i: NatIdentifier,
                               dt: DataType,
                               out: Phrase[AccType],
                               body: Phrase[`(nat)->`[AccType -> CommandType]])
  extends AbstractParForNat(n, i, dt, out, body) {

  def parallelismLevel: idealised.OpenCL.ParallelismLevel

  //  protected var env: OpenCLOldCodeGenerator.Environment = _

  def name: String

  def init: Nat
  def step: Nat
  def synchronize: Stmt
}

object OpenCLParForNat {
  def unapply(arg: OpenCLParForNat): Option[(Nat, NatIdentifier, DataType, Phrase[AccType], Phrase[`(nat)->`[AccType -> CommandType]])] =
    Some(arg.n, arg.i, arg.dt, arg.out, arg.body)
}
