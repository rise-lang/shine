package shine.OpenCL.IntermediatePrimitives

import arithexpr.arithmetic.ArithExpr.{intToCst, simplifyImplicitly}
import arithexpr.arithmetic.Log
import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative.acc
import shine.DPIA.DSL.{`new` => _, _}
import shine.DPIA.FunctionalPrimitives.{Cast, IndexAsNat, NatAsIndex, Split}
import shine.DPIA.ImperativePrimitives.PairAcc2
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType.idx
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL.{AdjustArraySizesForAllocations, get_local_id}
import shine.OpenCL.DSL._
import shine.OpenCL.ImperativePrimitives.ParForLocal

object OpenCLSegReduceI {
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
    val tree_loops: Nat = Log(2, o)

    val adj = AdjustArraySizesForAllocations(init, ArrayType(k, dt), addrSpace)

    comment("oclSegmentedReduce") `;`
    // Initialize final output array g_output
    `new` (addrSpace) (adj.dt, g_output =>
      acc(init)(adj.accF(g_output.wr)) `;`

        // Declare temporary output array s_data
        `new` (addrSpace) (ArrayType(o, pt), s_data =>

          // ********************************************************************
          // First Reduction: Every local thread reduces m elements sequentially.
          // ********************************************************************

          // Declare private variable for the reduction of the current segment
          `new` (AddressSpace.Private) (pt, current_reduction =>

            // Process all m (n/m)-sized chunks in parallel with all local threads
            MapLocalI(0)(o, pt, pt,
              λ(expT(ArrayType(m, pt), read))(x => λ(accT(pt))(a =>

                //TODO: Maybe add padding to avoid bank conflicts

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
                    //      Apparently this happens because the variable suffix rd isn't processed inside of acc.
                    (a :=| pt | current_reduction.rd)

                    // Atomic alternative:
                    // atomicBinOp(dt, f, g_output.wr `@` fst(current_reduction.rd), snd(current_reduction.rd)))


              ))),
              Split(m, o, read, pt, in),
              s_data.wr)

          ) `;`


          // ******************************************************************************************************
          // Second reduction: Reduce the remaining reduction elements of every thread with a tree-based algorithm.
          //                   (o = n / m in total, saved in the array s_data)
          // ******************************************************************************************************

            // Declare private variables for left and right elements of the current tree iteration plus their indices,
            // and a private variable for the stride for the tree-based access pattern.
            `new` (AddressSpace.Private) (pt, left_element =>
              `new` (AddressSpace.Private) (pt, right_element =>
                `new` (AddressSpace.Private) (int, left_index =>
                  `new` (AddressSpace.Private) (int, right_index =>
                    `new` (AddressSpace.Private) (int, stride =>

                  // Initialize stride with 1
                  acc(Literal(1))(stride.wr) `;`

                  // Loop Log(2, n / m) times
                  `for`(tree_loops, i =>

                    // Process o elements with all local threads.
                    // This always leads to inactive threads but is necessary because the loop iteration count
                    // must be equal to the size of the output array s_data (whose size is o).
                    // Note that the variable a isn't used here because you don't want to access s_data in a sequential
                    // but tree-based way, so you never use the expression a = s_data[lid].
                    ParForLocal(0)(o, pt, s_data.wr,
                      λ(expT(idx(o), read))(lid => λ(accT(pt))(a =>

                        //TODO: Maybe add a left and right shift operator?

                        // Make sure that only the first o / (2 * (i + 1)) threads are active in each iteration.
                        `if` (IndexAsNat(o, lid) `<`
                              (Natural(o) / (Natural(2) * Cast(int, NatType, stride.rd))))

                        `then` (

                          //TODO: Maybe add padding to avoid bank conflicts

                          //FIXME: Reading a NatType or IndexType in an assignment always leads to the error message
                          //       that the key can't be found in the environment. Therefore stride, left_index and
                          //       right_index are ints here and are later casted to NatTypes.

                          // Calculate the index of the left_element of the current iteration (2 * lid * stride + (stride - 1)))
                          (left_index.wr :=| int |
                            (Literal(2) * stride.rd * Cast(NatType, int, IndexAsNat(o, lid)) +
                              stride.rd - Literal(1))) `;`

                          // Calculate the index of the right_element of the current iteration (left_index + stride)
                          (right_index.wr :=| int | (left_index.rd + stride.rd)) `;`

                          //FIXME: Simply using a NatType variable inside NatAsIndex always leads to a key not found
                          //       error message (e.g. (s_data.rd `@` NatAsIndex(o, left_index.rd)), assuming
                          //       left_index is a NatType here). Strangely enough, using something like
                          //       Natural(1) * left_index.rd (again assuming left_index is a NatType) or in this case
                          //       Cast(int, NatType, left_index.rd) with left_index as an int won't throw an error.

                          // Save the left and right element in their private variables
                          (left_element.wr :=| pt |
                            (s_data.rd `@` NatAsIndex(o, Cast(int, NatType, left_index.rd)))) `;`
                          (right_element.wr :=| pt |
                            (s_data.rd `@` NatAsIndex(o, Cast(int, NatType, right_index.rd)))) `;`

                            // If segment of left_element != segment of right_element
                            (`if`(fst(left_element.rd) `!:=` fst(right_element.rd))

                             // Write left_element.value into g_output[left_element.key]
                             `then` f(g_output.rd `@` fst(left_element.rd))
                                     (snd(left_element.rd))
                                     (g_output.wr `@` fst(left_element.rd))

                             // Accumulate the value of left_element into the value of right_element
                             `else` f(snd(s_data.rd `@` NatAsIndex(o, Cast(int, NatType, right_index.rd))))
                                     (snd(left_element.rd))
                                     (PairAcc2(IndexType(k), dt,
                                       s_data.wr `@` NatAsIndex(o, Cast(int, NatType, right_index.rd))))
                            )
                        )

                        // Inactive threads do nothing.
                        `else` comment("do nothing"))

                    )) `;`

                    // Multiply the stride by 2
                    (stride.wr :=| int | Literal(2) * stride.rd) `;`

                    // Synchronize all local threads after each iteration.
                    barrier()
                )))))) `;`

          // After the for-loop is finished the final reduced element is still saved in s_data[o - 1],
          // so it still needs to be accumulated into g_output[s_data[o - 1].key].
          (`if` (Natural(get_local_id(0)) `=:=` Natural(0))
           `then` f(g_output.rd `@` fst(s_data.rd `@` NatAsIndex(o, Natural(o - 1))))
                  (snd(s_data.rd `@` NatAsIndex(o, Natural(o - 1))))
                  (g_output.wr `@` fst(s_data.rd `@` NatAsIndex(o, Natural(o - 1))))
           `else` comment("do nothing"))

        ) `;`

        barrier() `;`

        // Final result of the reduction of this workgroup is inside g_output.
        out(adj.exprF(g_output.rd))
      )
  }
}
