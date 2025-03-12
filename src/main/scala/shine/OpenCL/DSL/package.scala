package shine.OpenCL

import arithexpr.arithmetic.RangeAdd
import rise.core.types.{DataType, NatToData}
import rise.core.types.DataType._
import shine.DPIA.DSL._
import shine.DPIA.Phrases.{DepLambda, Phrase}
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL.primitives.imperative._

package object DSL {

  def parFor(level: ParallelismLevel,
             dim: Int,
             unroll: Boolean
            ): (Nat, DataType, Phrase[AccType], Phrase[FunType[ExpType, FunType[AccType, CommType]]]) => ParFor =
    level match {
      case Global =>    ParFor(level, dim, unroll, "gl_id_")(
        get_global_id(dim), _, get_global_size(dim), _, _, _)
      case Local =>     ParFor(level, dim, unroll,  "l_id_")(
        get_local_id(dim), _, get_local_size(dim), _, _, _)
      case WorkGroup => ParFor(level, dim, unroll, "wg_id_")(
        get_group_id(dim), _, get_num_groups(dim), _, _, _)
      case Sequential | Warp | Lane => throw new Exception("This should not happen")
    }

  def parForNat(level: ParallelismLevel,
                dim: Int,
                unroll: Boolean
               ): (Nat, NatToData, Phrase[AccType], Phrase[DepFunType[NatIdentifier, FunType[AccType, CommType]]]) => ParForNat =
    level match {
      case Global =>    ParForNat(level, dim, unroll, "gl_id_")(
        get_global_id(dim), _, get_global_size(dim), _, _, _)
      case Local =>     ParForNat(level, dim, unroll,  "l_id_")(
        get_local_id(dim), _, get_local_size(dim), _, _, _)
      case WorkGroup => ParForNat(level, dim, unroll, "wg_id_")(
        get_group_id(dim), _, get_num_groups(dim), _, _, _)
      case Sequential | Warp | Lane => throw new Exception("This should not happen")
    }

  private def parForBodyFunction(n:Nat, ft:NatToData,
                                 f:NatIdentifier => Phrase[AccType] => Phrase[CommType]
                                ): DepLambda[Nat, NatIdentifier, AccType ->: CommType] = {
    nFun(idx => fun(accT(ft(idx)))(o => f(idx)(o)), RangeAdd(0, n, 1))
  }

  def parForNatGlobal(dim:Int)(n:Nat, ft:NatToData, out:Phrase[AccType],
                               f: NatIdentifier => Phrase[AccType] => Phrase[CommType]): ParForNat =
    parForNat(Global, dim, unroll = false)(n, ft, out, parForBodyFunction(n, ft, f))

  def parForNatWorkGroup(dim:Int)(n:Nat, ft:NatToData, out:Phrase[AccType],
                                  f:NatIdentifier => Phrase[AccType] => Phrase[CommType]): ParForNat =
    parForNat(WorkGroup, dim, unroll = false)(n, ft, out, parForBodyFunction(n, ft, f))

  def parForNatLocal(dim:Int)(n:Nat, ft:NatToData, out:Phrase[AccType],
                              f:NatIdentifier => Phrase[AccType] => Phrase[CommType]): ParForNat =
    parForNat(Local, dim, unroll = false)(n, ft, out, parForBodyFunction(n, ft, f))

  object `new` {
    def apply(addrSpace: AddressSpace)
             (dt: DataType, f: Phrase[VarType] => Phrase[CommType]): New =
      New(addrSpace, dt, fun(varT(dt))(v => f(v) ))
  }

  object newDoubleBuffer {
    def apply(a: AddressSpace,
              dt1: DataType,
              dt2: DataType,
              dt3: ArrayType,
              in: Phrase[ExpType],
              out: Phrase[AccType],
              f: (Phrase[VarType], Phrase[CommType], Phrase[CommType]) => Phrase[CommType]): NewDoubleBuffer =
      NewDoubleBuffer(a, dt1, dt2, dt3.elemType, dt3.size, in, out,
        fun(varT(dt1) x CommType() x CommType())(ps => {
          val    v: Phrase[VarType]  = ps.`1`.`1`
          val swap: Phrase[CommType] = ps.`1`.`2`
          val done: Phrase[CommType] = ps.`2`
          f(v, swap, done)
        }))
  }

  object barrier {
    def apply(local: Boolean = true, global: Boolean = true): Barrier = Barrier(local, global)()
  }
}
