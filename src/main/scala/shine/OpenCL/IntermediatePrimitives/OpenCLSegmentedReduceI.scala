package shine.OpenCL.IntermediatePrimitives

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative.acc
import shine.DPIA.DSL.{`new` => _, _}
import shine.DPIA.FunctionalPrimitives.{NatAsIndex, Split}
import shine.DPIA.ImperativePrimitives.{PairAcc1, PairAcc2, SplitAcc}
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType.idx
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL.{AdjustArraySizesForAllocations, get_local_id}
import shine.OpenCL.DSL.{barrier, _}
import shine.OpenCL.ImperativePrimitives.ParForLocal

object OpenCLSegmentedReduceI {
  def apply(n: Nat,
            k: Nat,
            addrSpace: shine.DPIA.Types.AddressSpace,
            dt: DataType,
            f: Phrase[ExpType ->: ExpType ->: AccType ->: CommType],
            init: Phrase[ExpType],
            in: Phrase[ExpType],
            out: Phrase[ExpType ->: CommType])
           (implicit context: TranslationContext): Phrase[CommType] = {
    val pt = PairType(IndexType(k), dt)
    val m: Nat = 32

    val adj = AdjustArraySizesForAllocations(init, ArrayType(k, dt), addrSpace)

    comment("oclSegmentedReduce") `;`
    // Copy hist to addrSpace with all local threads
    `new` (addrSpace) (adj.dt, g_output =>
      acc(init)(adj.accF(g_output.wr)) `;`

        `new` (addrSpace) (ArrayType(m, pt), s_data =>
          `new` (AddressSpace.Private) (pt, current_element =>
            MapLocalI(0)(m, pt, pt,
              λ(expT(ArrayType(m, pt), read))(x => λ(accT(pt))(a =>
                acc(x `@` NatAsIndex(m, Natural(0)))(a)
              )),
              Split(m, m, read, pt, in),
              s_data.wr)

        /*// Copy in (is x xs) to addrSpace with all local threads
        `new` (addrSpace) (adj2.dt, s_data =>
          MapLocalI(0)(n, pt, pt,
            λ(expT(pt, read))(x => λ(accT(pt))(a => acc(x)(a))),
            in, s_data.wr)


          ParForLocal(0)(n, pt, s_data.wr,
            λ(expT(idx(n), read))(i => λ(accT(pt))(a =>
              acc(fst(in `@` i))(PairAcc1(IndexType(k), dt, a)) `;`
                acc(snd(in `@` i))(PairAcc2(IndexType(k), dt, a))))) `;`
            barrier()

            /*ParForLocal(0)(32, pt, s_data.wr,
              λ(expT(idx(n), read))(i => λ(accT(pt))(a =>
              )*/


          `new` (AddressSpace.Private) (NatType, lid =>
            acc(Natural(get_local_id(0)))(lid.wr) `;`
              `new` (AddressSpace.Private) (dt, current_red =>
                `new` (AddressSpace.Private) (IndexType(k), current_owner =>
                  `if` (lid.rd `<` Natural(32))
                  `then` acc(Natural(1))(current_red.wr)
                  `else` acc(Natural(0))(current_red.wr)
          ))) `;`*/

          )) `;`
          out(adj.exprF(g_output.rd))
      )
  }
}
