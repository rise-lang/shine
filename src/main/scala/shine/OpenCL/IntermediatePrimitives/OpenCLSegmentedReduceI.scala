package shine.OpenCL.IntermediatePrimitives

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative.acc
import shine.DPIA.DSL.{`new` => _, _}
import shine.DPIA.FunctionalPrimitives.{IndexAsNat, NatAsIndex, Split}
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
    val o: Nat = n/m

    val adj = AdjustArraySizesForAllocations(init, ArrayType(k, dt), addrSpace)

    comment("oclSegmentedReduce") `;`
    // Initialize final output array g_output
    `new` (addrSpace) (adj.dt, g_output =>
      acc(init)(adj.accF(g_output.wr)) `;`

        // Declare temporary output array s_data
        `new` (addrSpace) (ArrayType(o, pt), s_data =>
          // Declare private variable for the reduction of the current segment
          `new` (AddressSpace.Private) (pt, current_reduction =>

            // Process all m (n/m)-sized chunks
            MapLocalI(0)(o, pt, pt,
              λ(expT(ArrayType(m, pt), read))(x => λ(accT(pt))(a =>

                // Process first element x[0]
                acc(x `@` NatAsIndex(m, Natural(0)))(current_reduction.wr) `;`
                  // Declare private variable for the element of the current for-loop iteration
                  `new` (AddressSpace.Private) (pt, current_element =>

                    // Loop over the remaining (m - 1) elements
                    `for`(m - 1, i =>
                      // Save current element (x[i + 1]) in current_element
                      acc(x `@` NatAsIndex(m, IndexAsNat(m - 1, i) + Natural(1)))(current_element.wr) `;`

                       // If segment of current_reduction != segment of current_element
                        (`if` (fst(current_reduction.rd) `!:=` fst(current_element.rd))

                          // FIXME: Doesn't work for multiple commands (second command out of if-brackets)
                         `then` (
                            // => end of current segment reached
                            // Write current_reduction.value into g_output[current_reduction.key]
                            f(g_output.rd `@` fst(current_reduction.rd))
                             (snd(current_reduction.rd))
                             (g_output.wr `@` fst(current_reduction.rd)) `;`

                            // and assign current_element to current_reduction
                            (current_reduction.wr :=| pt | current_element.rd)
                          )

                        // Accumulate the value of current_element into the value of current_reduction
                        `else` f(snd(current_reduction.rd))
                                (snd(current_element.rd))
                                (PairAcc2(IndexType(k), dt, current_reduction.wr)))
                      ) `;`

                    //TODO: This command works but acc(current_element.rd)(a) doesn't.
                    // Apparently this happens because the variable suffix rd isn't processed inside of acc.
                    (a :=| pt | current_reduction.rd)

              ))),
              Split(m, o, read, pt, in),
              s_data.wr)

          )) `;`

          out(adj.exprF(g_output.rd))
      )
  }
}
