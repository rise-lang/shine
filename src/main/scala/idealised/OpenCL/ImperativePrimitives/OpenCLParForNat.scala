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
                               body: Phrase[`(nat)->:`[AccType ->: CommType]],
                               val init: Nat,
                               val step: Nat,
                               val unroll: Boolean)
  extends AbstractParForNat(n, ft, out, body) {

  // TODO: This should not extend OpenMP's par for
  // it should include init and step in visitAndRebuild
  assert(false)

  def parallelismLevel: idealised.OpenCL.ParallelismLevel

  //  protected var env: OpenCLOldCodeGenerator.Environment = _

  def name: String
}

object OpenCLParForNat
{
  def unapply(arg: OpenCLParForNat): Option[(Nat, NatToData, Phrase[AccType], Phrase[`(nat)->:`[AccType ->: CommType]], Nat, Nat, Boolean)] = {
    Some((arg.n, arg.ft, arg.out, arg.body, arg.init, arg.step, arg.unroll))
  }
}
