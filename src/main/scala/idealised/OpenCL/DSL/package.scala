package idealised.OpenCL
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenCL.ImperativePrimitives.{ParForNatGlobal, ParForNatLocal, ParForNatWorkGroup, OpenCLNew}
import lift.arithmetic.RangeAdd

package object DSL {

  private def parForBodyFunction(n:Nat, i:NatIdentifier, dt:DataType, f:NatIdentifier => Phrase[AccType] => Phrase[CommandType]) = {
    def makeDt(x:Nat) = DataType.substitute(x, `for`=i, `in`=dt)
    _Λ_(idx => λ(acc"[${makeDt(idx)}]")(o => f(idx)(o)), RangeAdd(0, n, 1))
  }

  object parForNatGlobal {
    def apply(dim:Int)(n:Nat, i:NatIdentifier, dt:DataType, out:Phrase[AccType], f:NatIdentifier => Phrase[AccType] => Phrase[CommandType]):ParForNatGlobal = {
      ParForNatGlobal(dim)(n, i,  dt, out, parForBodyFunction(n, i, dt, f))
    }
  }

  object parForNatWorkGroup {
    def apply(dim:Int)(n:Nat, i:NatIdentifier, dt:DataType, out:Phrase[AccType], f:NatIdentifier => Phrase[AccType] => Phrase[CommandType]):ParForNatWorkGroup = {
      ParForNatWorkGroup(dim)(n, i,  dt, out, parForBodyFunction(n, i, dt, f))
    }
  }

  object parForNatLocal {
    def apply(dim:Int)(n:Nat, i:NatIdentifier, dt:DataType, out:Phrase[AccType], f:NatIdentifier => Phrase[AccType] => Phrase[CommandType]):ParForNatLocal = {
      ParForNatLocal(dim)(n, i,  dt, out,  parForBodyFunction(n, i, dt, f))
    }
  }

  object newWithAddrSpace {
    def apply(dt: DataType,
              addressSpace: AddressSpace,
              f: Phrase[VarType -> CommandType]): OpenCLNew =
      OpenCLNew(dt, addressSpace, f)

    def apply(dt: DataType,
              addressSpace: AddressSpace,
              f: Phrase[VarType] => Phrase[CommandType]): OpenCLNew =
      OpenCLNew(dt, addressSpace, λ(exp"[$dt]" x acc"[$dt]")(v => f(v) ))
  }
}
