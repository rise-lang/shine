package idealised.OpenCL.ImperativePrimitives

import idealised.C.AST.Stmt
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types._
import idealised.DPIA.{->:, Nat, `(nat)->:`}
import idealised.OpenCL.BuiltInFunctionCall
import idealised.OpenMP.ImperativePrimitives.AbstractParForNat

abstract class OpenCLParForNat(n: Nat,
                               ft:NatToData,
                               out: Phrase[AccType],
                               body: Phrase[`(nat)->:`[AccType ->: CommType]])
  extends AbstractParForNat(n, ft, out, body) {

  def parallelismLevel: idealised.OpenCL.ParallelismLevel

  //  protected var env: OpenCLOldCodeGenerator.Environment = _

  def name: String

  def init: BuiltInFunctionCall
  def step: BuiltInFunctionCall
  def synchronize: Stmt
}

object OpenCLParForNat
{
  def unapply(arg: OpenCLParForNat): Option[(Nat, NatToData, Phrase[AccType], Phrase[`(nat)->:`[AccType ->: CommType]])] = {
    Some(arg.n, arg.ft, arg.out, arg.body)
  }
}
