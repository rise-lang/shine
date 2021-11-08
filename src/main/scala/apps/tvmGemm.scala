package apps

import elevate.core._
import elevate.core.strategies.basic._
import elevate.core.strategies.traversal._
import rise.core.DSL._
import rise.core.primitives._
import rise.core.types._
import rise.elevate._
import rise.elevate.rules.algorithmic._
import rise.elevate.rules.lowering._
import rise.elevate.rules.traversal._
import rise.elevate.rules.traversal.default._
import rise.elevate.strategies.algorithmic.reorder
import rise.elevate.strategies.lowering._
import rise.elevate.strategies.normalForm._
import rise.elevate.strategies.predicate._
import rise.elevate.strategies.tiling._
import rise.elevate.strategies.traversal
import rise.elevate.strategies.traversal._

object tvmGemm {
  val outermost: (Strategy[Rise]) => (Strategy[Rise]) => Strategy[Rise] =
    traversal.outermost(alternative2.RiseTraversable)
  val innermost: (Strategy[Rise]) => (Strategy[Rise]) => Strategy[Rise] =
    traversal.innermost(alternative2.RiseTraversable)

  //// MM INPUT EXPRESSION /////////////////////////////////////////////////////
  val N = 1024

  val mm: Rise = //infer(
    fun(ArrayType(N, ArrayType(N, f32)))(a =>
      fun(ArrayType(N, ArrayType(N, f32)))(b =>
        a |> map(fun(ak =>
          transpose(b) |> map(fun(bk =>
            zip(ak)(bk) |>
              map(fun(x => fst(x) * snd(x))) |>
              reduce(add)(lf32(0.0f))
          ))
        ))
      ))
  //)

  //// ICFP'20 TVM - STRATEGIES ////////////////////////////////////////////////
  // -- BASELINE ---------------------------------------------------------------

  val baseline: Strategy[Rise] = DFNF()(default.RiseTraversable) `;`
    (fuseReduceMap `@` topDown[Rise])

  // -- BLOCKING ---------------------------------------------------------------

  val isFullyAppliedReduce: Strategy[Rise] = isApplied(isApplied(isApplied(isReduce)))
  val blocking: Strategy[Rise] =
    baseline `;`
      (tile(32,32)        `@` outermost(mapNest(2))) `;;`
      (reduceMapFission() `@` outermost(isApplied(isApplied(isReduceSeq)))) `;;`
      (splitStrategy(4)   `@` innermost(isFullyAppliedReduce)) `;;`
      reorder(List(1,2,5,6,3,4))

  val blockingPartial1: Strategy[Rise] =
    baseline `;`
      (tile(32,32)        `@` outermost(mapNest(2)))
  val blockingPartial2: Strategy[Rise] =
    baseline `;`
      (tile(32,32)        `@` outermost(mapNest(2))) `;;`
      (reduceMapFission() `@` outermost(isApplied(isApplied(isReduceSeq))))
  val blockingPartial3: Strategy[Rise] =
    baseline `;`
      (tile(32,32)        `@` outermost(mapNest(2))) `;;`
      (reduceMapFission() `@` outermost(isApplied(isApplied(isReduceSeq)))) `;;`
      (splitStrategy(4)   `@` innermost(isFullyAppliedReduce))

  // -- VECTORIZATION ----------------------------------------------------------

  val isFullyAppliedMap: Strategy[Rise] = isApplied(isApplied(isMap))
  val vectorization: Strategy[Rise] =
    blocking `;;`
      (vectorize(32) `@` innermost(isApplied(isApplied(isMap))))

  // -- LOOP PERMUTATION -------------------------------------------------------

  val loopPerm: Strategy[Rise] = baseline `;`
    (tile(32,32)        `@` outermost(mapNest(2))) `;;`
    (reduceMapFission() `@` outermost(isApplied(isApplied(isReduceSeq)))) `;;`
    (splitStrategy(4)   `@` innermost(isFullyAppliedReduce)) `;;`
    reorder(List(1,2,5,3,6,4)) `;;`
    (vectorize(32) `@` innermost(isFullyAppliedMap))

  // -- ARRAY PACKING ----------------------------------------------------------

  val isTransposedB: Strategy[Rise] = isApplied(isTranspose)
  val permuteB: Strategy[Rise] =
    splitJoin2(32) `;` DFNF() `;` argument(idAfter) `;`
      topDown(liftId()) `;` topDown(createTransposePair) `;` RNF() `;`
      argument(argument(idAfter)) `;` normalize.apply(liftId()) `;`
      topDown(idToCopy)

  val packB: Strategy[Rise] =
    storeInMemory(isTransposedB,
      permuteB `;;`
        (vectorize(32) `@` innermost(isFullyAppliedMap)) `;;`
        (parallel()    `@` outermost(isApplied(isMap)))
    ) `@` inLambda

  def inLambda(s: Strategy[Rise]): Strategy[Rise] =
    isLambda `;` ( (e: Rise) => body(inLambda(s))(e) ) <+ s

  val arrayPacking: Strategy[Rise] = packB `;;` loopPerm

  // -- CACHE BLOCKS -----------------------------------------------------------

  val cacheBlocks: Strategy[Rise] = (
    arrayPacking `;;` // elevate.core.strategies.debug.debug[Rise]("after arrayPacking") `;`
      (unroll `@` innermost(isReduceSeq))
    )

  // -- PARALLEL ---------------------------------------------------------------

  val par = (
    arrayPacking `;;`
      (parallel() `@` outermost(isApplied(isMap))) `;;`
      (unroll `@` innermost(isReduceSeq))
    )
}
