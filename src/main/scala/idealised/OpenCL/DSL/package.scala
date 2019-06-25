package idealised.OpenCL
import idealised.DPIA.DSL._
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenCL.ImperativePrimitives.{ParForNatGlobal, ParForNatLocal, ParForNatWorkGroup, OpenCLNew}
import lift.arithmetic.RangeAdd

package object DSL {

  private def parForBodyFunction(n:Nat, ft:NatDataTypeFunction, f:NatIdentifier => Phrase[AccType] => Phrase[CommandType]) = {
    _Λ_(idx => λ(acc"[${ft(idx)}]")(o => f(idx)(o)), RangeAdd(0, n, 1))
  }

  object parForNatGlobal {
    def apply(dim:Int)(n:Nat, ft:NatDataTypeFunction, out:Phrase[AccType], f:NatIdentifier => Phrase[AccType] => Phrase[CommandType]):ParForNatGlobal = {
      ParForNatGlobal(dim)(n, ft, out, parForBodyFunction(n, ft, f))
    }
  }

  object parForNatWorkGroup {
    def apply(dim:Int)(n:Nat, ft:NatDataTypeFunction, out:Phrase[AccType], f:NatIdentifier => Phrase[AccType] => Phrase[CommandType]):ParForNatWorkGroup = {
      ParForNatWorkGroup(dim)(n, ft, out, parForBodyFunction(n, ft, f))
    }
  }

  object parForNatLocal {
    def apply(dim:Int)(n:Nat, ft:NatDataTypeFunction, out:Phrase[AccType], f:NatIdentifier => Phrase[AccType] => Phrase[CommandType]):ParForNatLocal = {
      ParForNatLocal(dim)(n, ft, out,  parForBodyFunction(n, ft, f))
    }
  }

  object newWithAddrSpace {
    def apply(addrSpace: AddrSpace,
              dt: DataType,
              f: Phrase[VarType -> CommandType]): OpenCLNew =
      OpenCLNew(addrSpace, dt, f)

    def apply(addrSpace: AddrSpace,
              dt: DataType,
              f: Phrase[VarType] => Phrase[CommandType]): OpenCLNew =
      OpenCLNew(addrSpace, dt, λ(exp"[$dt, $Read]" x acc"[$dt]")(v => f(v) ))
  }
}
