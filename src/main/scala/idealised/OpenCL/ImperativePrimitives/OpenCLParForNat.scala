package idealised.OpenCL.ImperativePrimitives

import idealised.C.AST.Stmt
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types.{AccType, CommandType, DataType}
import idealised.DPIA.{->, Nat, NatDataTypeFunction, NatIdentifier, `(nat)->`}
import idealised.OpenCL.BuiltInFunction
import idealised.OpenMP.ImperativePrimitives.AbstractParForNat

abstract class OpenCLParForNat(n: Nat,
                               ft:NatDataTypeFunction,
                               out: Phrase[AccType],
                               body: Phrase[`(nat)->`[AccType -> CommandType]])
  extends AbstractParForNat(n, ft, out, body) {

  def parallelismLevel: idealised.OpenCL.ParallelismLevel

  //  protected var env: OpenCLOldCodeGenerator.Environment = _

  def name: String

  def init: BuiltInFunction
  def step: BuiltInFunction
  def synchronize: Stmt
}

object OpenCLParForNat
{
  def unapply(arg: OpenCLParForNat): Option[(Nat, NatDataTypeFunction, Phrase[AccType], Phrase[`(nat)->`[AccType -> CommandType]])] = {
    Some(arg.n, arg.ft, arg.out, arg.body)
  }
}
