package idealised.OpenCL
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenCL.ImperativePrimitives.{ParForNatGlobal, ParForNatLocal, ParForNatWorkGroup, OpenCLNew}
import lift.arithmetic.RangeAdd

package object DSL {

  private def parForBodyFunction(n:Nat, ft:NatDataTypeFunction, f:NatIdentifier => Phrase[AccType] => Phrase[CommType]) = {
    _Λ_(idx => λ(acc"[${ft(idx)}]")(o => f(idx)(o)), RangeAdd(0, n, 1))
  }

  object parForNatGlobal {
    def apply(dim:Int)(n:Nat, ft:NatDataTypeFunction, out:Phrase[AccType], f:NatIdentifier => Phrase[AccType] => Phrase[CommType]):ParForNatGlobal = {
      ParForNatGlobal(dim)(n, ft, out, parForBodyFunction(n, ft, f))
    }
  }

  object parForNatWorkGroup {
    def apply(dim:Int)(n:Nat, ft:NatDataTypeFunction, out:Phrase[AccType], f:NatIdentifier => Phrase[AccType] => Phrase[CommType]):ParForNatWorkGroup = {
      ParForNatWorkGroup(dim)(n, ft, out, parForBodyFunction(n, ft, f))
    }
  }

  object parForNatLocal {
    def apply(dim:Int)(n:Nat, ft:NatDataTypeFunction, out:Phrase[AccType], f:NatIdentifier => Phrase[AccType] => Phrase[CommType]):ParForNatLocal = {
      ParForNatLocal(dim)(n, ft, out,  parForBodyFunction(n, ft, f))
    }
  }

  object newWithAddrSpace {
    def apply(dt: DataType,
              addressSpace: AddressSpace,
              f: Phrase[VarType -> CommType]): OpenCLNew =
      OpenCLNew(dt, addressSpace, f)

    def apply(dt: DataType,
              addressSpace: AddressSpace,
              f: Phrase[VarType] => Phrase[CommType]): OpenCLNew =
      OpenCLNew(dt, addressSpace, λ(exp"[$dt]" x acc"[$dt]")(v => f(v) ))
  }
}
