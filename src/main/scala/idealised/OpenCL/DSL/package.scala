package idealised.OpenCL

import idealised.DPIA.DSL._
import idealised.DPIA.Phrases.Phrase
import idealised.DPIA.Types._
import idealised.DPIA._
import idealised.OpenCL.ImperativePrimitives._
import lift.arithmetic.RangeAdd

package object DSL {

  private def parForBodyFunction(n:Nat, ft:NatToData, f:NatIdentifier => Phrase[AccType] => Phrase[CommType]) = {
    nFun(idx => λ(acc"[${ft(idx)}]")(o => f(idx)(o)), RangeAdd(0, n, 1))
  }

  object parForNatGlobal {
    def apply(dim:Int)(n:Nat, ft:NatToData, out:Phrase[AccType], f:NatIdentifier => Phrase[AccType] => Phrase[CommType]):ParForNatGlobal = {
      ParForNatGlobal(dim)(n, ft, out, parForBodyFunction(n, ft, f))
    }
  }

  object parForNatWorkGroup {
    def apply(dim:Int)(n:Nat, ft:NatToData, out:Phrase[AccType], f:NatIdentifier => Phrase[AccType] => Phrase[CommType]):ParForNatWorkGroup = {
      ParForNatWorkGroup(dim)(n, ft, out, parForBodyFunction(n, ft, f))
    }
  }

  object parForNatLocal {
    def apply(dim:Int)(n:Nat, ft:NatToData, out:Phrase[AccType], f:NatIdentifier => Phrase[AccType] => Phrase[CommType]):ParForNatLocal = {
      ParForNatLocal(dim)(n, ft, out,  parForBodyFunction(n, ft, f))
    }
  }

  object `new` {
    def apply(addrSpace: idealised.DPIA.Types.AddressSpace)
             (dt: DataType, f: Phrase[VarType] => Phrase[CommType]): OpenCLNew =
      OpenCLNew(addrSpace, dt, λ(exp"[$dt, $read]" x acc"[$dt]")(v => f(v) ))
  }

  object newDoubleBuffer {
    def apply(a: AddressSpace,
              dt1: DataType,
              dt2: DataType,
              dt3: ArrayType,
              in: Phrase[ExpType],
              out: Phrase[AccType],
              f: (Phrase[VarType], Phrase[CommType], Phrase[CommType]) => Phrase[CommType]) =
      OpenCLNewDoubleBuffer(a, dt1, dt2, dt3.elemType, dt3.size, in, out, λ(varT"[$dt1]" x CommType() x CommType())(ps => {
        val    v: Phrase[VarType]  = ps._1._1
        val swap: Phrase[CommType] = ps._1._2
        val done: Phrase[CommType] = ps._2
        f(v, swap, done)
      }))
  }
}
