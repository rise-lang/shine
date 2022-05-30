package exploration.strategies

import elevate.core.{Strategy, Success}
import elevate.core.strategies.traversal._
import elevate.macros.RuleMacro.rule
import exploration.strategies.simpleStrategiesGPU.{allSplitJoin, allTopdownSplitJoin, bottomUpSplitJoin, lowerGs0, lowerGs1, lowerGsGs, lowerLcl0, lowerLcl1, lowerWrg0, lowerWrg1, lowerWrgLcl, lowerWrgWrgLclLcl, oneSplitJoin, oneUsingStateSplitJoin, someSplitJoin, topDownSplitJoin}
import rise.core.App
import rise.core.DSL.{TypeAssertionHelper, preserveType}
import rise.core.primitives.map
import rise.core.types.DataType
import rise.elevate.rules.algorithmic._
import rise.elevate.rules.lowering.{addRequiredCopies, reduceOCL}
import rise.elevate.rules.traversal.default.RiseTraversable
import rise.elevate.{Rise, tunable}
import rise.eqsat.NamedRewriteDSL.{app, lam, map}
import rise.eqsat.{NamedRewrite, rules}
//import rise.eqsat.rules.{Rule, liftReduceSeq, liftReduceSeq2, liftReduceSeq3, mapFission, reduceSeq, reduceSeqMapFission, reduceSeqMapFusion, splitBeforeMap, splitJoin2M, transposeAroundMapMapF1M}

//import rise.eqsat.rules._

/*

Thomas Koehler: The first thing I would do is to change the starting expression to :

    (depFun((n: Nat, m: Nat) => fun(
      (m `.` n `.` f32) ->: (n `.` f32) ->: (m `.` f32)
    )((mat, xs) =>
      mat |> map(fun(row =>
        zip(row)(xs) |>
        map(fun(x => fst(x) * snd(x))) |>
        reduce(add)(lf32(0.0f))
      ))
    ))
Thomas Koehler: to make it high-level

Thomas Koehler: with split and reorder rules I can get

 Λn0:nat Λn1:nat λx0 λx1 join
                         ┕ map
                           ┝ λx2 λx3 reduceSeq
                           │     │   ┝ λx4 λx5 map
                           │     │   │         ┝ λx6 λx7 λx8 add x7
                           │     │   │         │     │       ┕ reduceSeq <λx9. λx10. add x9 (mul (fst x10) (snd x10))> 0.0000 x8
                           │     │   │         │     ┝ fst x6
                           │     │   │         │     ┕ snd x6
                           │     │   │         ┕ zip x4 x5
                           │     │   ┝ generate <λx4. 0.0000>
                           │     │   ┕ transpose x3
                           │     ┕ map <λx3. split 32 (zip x3 x1)> x2
                           ┕ split 32 x0
Thomas Koehler: Then hopefully rules to copy memory and lower patterns might do the trick

Thomas Koehler: I'll give you the split/reorder rules that I use, I'll look into the copy/lower things later

Thomas Koehler: mapFission, reduceSeq, eliminateMapIdentity, reduceSeqMapFusion, splitJoin, splitJoin2M, blockedReduce, splitBeforeMap, reduceSeqMapFission, liftReduceSeq, liftReduceSeq2, liftReduceSeq3, transposeAroundMapMapF1M
https://github.com/rise-lang/shine/blob/eqsat-filter-extract/src/main/scala/rise/eqsat/rules.scala

Thomas Koehler: there is not guarantee that all of these rules are required, some of them might be unused by my search

Thomas Koehler: but with these rules you can get the split/reorder result I pasted above (I think it is getting algorithmically quite close to your low level program)

Thomas Koehler: some of these rules are also shortcuts (e.g. splitJoin2M could be inferred by splitJoin, mapFusion, mapFission or something like that)



 */


/*

Thomas Koehler: So the difficulty now is that the input to the inner reduceSeq is snd x5 which is looking different from zip(snd(x))(localX = snd(next)). I think basically what we want to store to memory is snd unzip snd x5, which we should also somehow know is constant in the outer map loop over x5 .. tricky.

Thomas Koehler: Currently I can find this:

 Λn0:nat Λn1:nat λx0 λx1 join
                         ┕ mapWorkGroup
                           ┝ λx2 mapLocal <λx3. x3>
                           │     ┕ oclReduceSeq Local
                           │       ┝ λx4 λx5 mapLocal
                           │       │         ┝ λx6 let (oclToMem Local (mapLocal <λx9. x9> (snd x6)))
                           │       │         │     ┕ λx10 add (fst x6)
                           │       │         │            ┕ oclReduceSeq Local
                           │       │         │              ┝ λx7. λx8. add x7 (mul (fst x8) (snd x8))
                           │       │         │              ┝ 0.0000
                           │       │         │              ┕ x10
                           │       │         ┕ zip x4 x5
                           │       ┝ mapLocal <λx11. x11> (generate <λx12. 0.0000>)
                           │       ┕ transpose (map <λx13. split 32 (zip x13 x1)> x2)
                           ┕ split 32 x0
Thomas Koehler: but instead of doing something like:

    for (int i_2548 = 0;(i_2548 < (n59 / 32));i_2548 = (1 + i_2548)) {
      /* mapLocal */
      for (int l_id_2549 = get_local_id(0);(l_id_2549 < 32);l_id_2549 = (l_id_2549 + get_local_size(0))) {
        x2500[l_id_2549] = e62[(l_id_2549 + (32 * i_2548))];
      }

      barrier(CLK_LOCAL_MEM_FENCE);
      /* mapLocal */
      for (int l_id_2550 = get_local_id(0);(l_id_2550 < 32);l_id_2550 = (l_id_2550 + get_local_size(0))) {
it does something like:

    for (int i_3001 = 0;(i_3001 < (n0 / 32));i_3001 = (1 + i_3001)) {
      /* mapLocal */
      for (int l_id_3002 = get_local_id(0);(l_id_3002 < 32);l_id_3002 = (l_id_3002 + get_local_size(0))) {
        /* mapLocal */
        for (int l_id_3003 = get_local_id(0);(l_id_3003 < 32);l_id_3003 = (l_id_3003 + get_local_size(0))) {
          x2952[(l_id_3003 + (32 * get_local_id(0)))]._fst = x0[(((l_id_3003 + (32 * i_3001)) + ((32 * n0) * wg_id_2999)) + (l_id_3002 * n0))];
          x2952[(l_id_3003 + (32 * get_local_id(0)))]._snd = x1[(l_id_3003 + (32 * i_3001))];
        }
Thomas Koehler: so the fst projection is stored to memory where we do not want to, and the snd projection is stored at the wrong loop nest.

Thomas Koehler: I'll give you the rewrite rules that got me there already, but there a bit of tricky stuff to do still.

Thomas Koehler: ocl.toMem(rct.AddressSpace.Local), ocl.mapArray, mapFusion, ocl.reduceSeq(rct.AddressSpace.Local), ocl.reduceSeq2(rct.AddressSpace.Local), ocl.mapWorkGroup(0), ocl.mapLocal(0).
https://github.com/rise-lang/shine/blob/sges-mvblast/src/main/scala/rise/eqsat/rules.scala
 */

object mvStrategiesGPU {

  // what about traversals? Where to apply these rules?

  // extracted
  //  val test = Success(mapFission)

  //  def mapFission2: Strategy[Rise] = `*g >> *f -> *(g >> f)`
  //  @rule def `*g >> *f -> *(g >> f)`: Strategy[Rise] = {
  //    case e @ App(App(map(), f), App(App(map(), g), arg)) =>
  //      Success(map(preserveType(g) >> f)(arg) !: e.t)
  //  }
  //
  //
  //  val strategies3 = Set[Strategy[Rise]](
  //    mapFusion,
  //    mapFission,
  //    reduceSeq,
  //    reduceSeqMapFusion,
  //    tunable(splitJoin),
  //    tunable(splitStrategy),
  //    splitBeforeMap,
  //    reduceSeqMapFission,
  //    liftReduceSeq,
  //    liftReduceSeq2,
  //    liftReduceSeq3,
  //    transposeAroundMapMapF1M,
  //
  //    //     partial lowerings
  //    lowerGs0,
  //    lowerGs1,
  //    lowerWrg0,
  //    lowerWrg1,
  //    lowerLcl0,
  //    lowerLcl1,
  //    lowerGsGs,
  //    lowerWrgLcl,
  //    lowerWrgWrgLclLcl,
  //    //     split join
  //    allSplitJoin,
  //    oneSplitJoin,
  //    someSplitJoin,
  //    oneUsingStateSplitJoin,
  //    topDownSplitJoin,
  //    allTopdownSplitJoin,
  //    bottomUpSplitJoin
  //
  //  )


  //  // list
  //  mapFission,
  //  reduceSeq,
  //  eliminateMapIdentity,
  //  reduceSeqMapFusion,
  //  splitJoin,
  //  splitJoin2M,
  //  blockedReduce,
  //  splitBeforeMap,
  //  reduceSeqMapFission,
  //  liftReduceSeq,
  //  liftReduceSeq2,
  //  liftReduceSeq3,
  //  transposeAroundMapMapF1M
  //
  //  ocl.toMem(rct.AddressSpace.Local),
  //  ocl.mapArray,
  //  mapFusion,
  //  ocl.reduceSeq(rct.AddressSpace.Local),
  //  ocl.reduceSeq2(rct.AddressSpace.Local),
  //  ocl.mapWorkGroup(0),
  //  ocl.mapLocal(0)

}
