package idealised.OpenCL
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenCL.ImperativePrimitives.ParForNatGlobal
import lift.arithmetic.RangeAdd
package object DSL {
  object parForNatGlobal {
    def apply(dim:Int)(n:Nat, i:NatIdentifier, dt:DataType, out:Phrase[AccType], f:NatIdentifier => Phrase[AccType] => Phrase[CommandType]):ParForNatGlobal = {
      def makeDt(x:Nat) = DataType.substitute(x, `for`=i, `in`=dt)
      ParForNatGlobal(dim)(n, i,  dt, out, _Λ_(idx => λ(acc"[${makeDt(idx)}]")(o => f(idx)(o)), RangeAdd(0, n, 1)))
    }
  }
}
