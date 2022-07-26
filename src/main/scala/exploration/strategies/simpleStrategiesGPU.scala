package exploration.strategies

import elevate.core.Strategy
import rise.elevate.rules.algorithmic._
import rise.elevate.rules.lowering.{addRequiredCopies, reduceOCL}
import rise.elevate.rules.traversal.default.RiseTraversable
import rise.elevate.{Rise, tunable}
import elevate.macros.RuleMacro.rule
import elevate.core.strategies.traversal._

object simpleStrategiesGPU {

  // maps inside map reduce will stay maps instead of mapSeqs
  val lowering =
    addRequiredCopies() `;`
      fuseReduceMap2 `;` // fuse map and reduce
      rise.elevate.rules.lowering.specializeSeq() `;` // lower: map -> mapSeq, reduce -> reduceSeq
      reduceMapFission2 `;` // fission map and reduce
      rise.elevate.rules.lowering.specializeSeqReduce() `;` // lower: reduce -> reduceSeq
      reduceOCL() // lower: reduceSeq -> oclReduceSeq(AddressSpace.Private)

  // strategies
  @rule def allSplitJoin: Strategy[Rise] =
    all(tunable(splitJoin))

  @rule def oneSplitJoin: Strategy[Rise] =
    one(tunable(splitJoin))

  @rule def someSplitJoin: Strategy[Rise] =
    some(tunable(splitJoin))

  @rule def oneUsingStateSplitJoin: Strategy[Rise] =
    oneUsingState(tunable(splitJoin))

  @rule def topDownSplitJoin: Strategy[Rise] =
    topDown(tunable(splitJoin))

  @rule def allTopdownSplitJoin: Strategy[Rise] =
    allTopdown(tunable(splitJoin))

  @rule def bottomUpSplitJoin: Strategy[Rise] =
    bottomUp(tunable(splitJoin))

  // lowerings
  @rule def lowerGs0: Strategy[Rise] =
    topDown(rise.elevate.rules.lowering.mapGlobal(0))

  @rule def lowerGs1: Strategy[Rise] =
    topDown(rise.elevate.rules.lowering.mapGlobal(1))

  @rule def lowerWrg0: Strategy[Rise] =
    topDown(rise.elevate.rules.lowering.mapWorkGroup(0))

  @rule def lowerWrg1: Strategy[Rise] =
    topDown(rise.elevate.rules.lowering.mapWorkGroup(1))

  @rule def lowerLcl0: Strategy[Rise] =
    topDown(rise.elevate.rules.lowering.mapLocal(0))

  @rule def lowerLcl1: Strategy[Rise] =
    topDown(rise.elevate.rules.lowering.mapLocal(1))

  @rule def lowerGsGs: Strategy[Rise] =
    topDown(rise.elevate.rules.lowering.mapGlobal(0)) `;`
      topDown(rise.elevate.rules.lowering.mapGlobal(1))

  @rule def lowerWrgLcl: Strategy[Rise] =
    topDown(rise.elevate.rules.lowering.mapWorkGroup(0)) `;`
      topDown(rise.elevate.rules.lowering.mapLocal(0))

  @rule def lowerWrgWrgLclLcl: Strategy[Rise] =
    topDown(rise.elevate.rules.lowering.mapWorkGroup(0)) `;`
      topDown(rise.elevate.rules.lowering.mapWorkGroup(1)) `;`
      topDown(rise.elevate.rules.lowering.mapLocal(0)) `;`
      topDown(rise.elevate.rules.lowering.mapLocal(1))

  val strategies2 = scala.collection.immutable.Seq[Strategy[Rise]](
    topDownSplitJoin,
  )

  val strategies = scala.collection.immutable.Seq[Strategy[Rise]](
    //     partial lowerings
    lowerGs0,
    lowerGs1,
    lowerWrg0,
    lowerWrg1,
    lowerLcl0,
    lowerLcl1,
    lowerGsGs,
    lowerWrgLcl,
    lowerWrgWrgLclLcl,
    //     split join
    allSplitJoin,
    oneSplitJoin,
    someSplitJoin,
    oneUsingStateSplitJoin,
    topDownSplitJoin,
    allTopdownSplitJoin,
    bottomUpSplitJoin
  )
}
