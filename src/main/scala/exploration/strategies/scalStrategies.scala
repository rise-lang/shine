package exploration.strategies
import elevate.core.Strategy
import elevate.core.strategies.debug
import elevate.core.strategies.traversal.{allTopdown, alltd, bottomUp, one, some, somebu, sometd, topDown, tryAll}
import exploration.strategies.scalStrategies.sjtd
import rise.elevate.{Rise, tunable}
import rise.elevate.rules.algorithmic.{createTransposePair, fuseReduceMap, fuseReduceMap2, idAfter, mapFusion, mapLastFission, reduceMapFission, reduceMapFusion, removeTransposePair, splitJoin, transposePairAfter}
import rise.elevate.rules.lowering.{addRequiredCopies, reduceOCL}
import rise.elevate.rules.traversal.default.RiseTraversable
import elevate.core.strategies.traversal._
import elevate.macros.RuleMacro.rule
import rise.eqsat.Nat

import scala.collection.mutable.ListBuffer

object scalStrategies {

  val lowerAll = addRequiredCopies() `;` rise.elevate.rules.lowering.specializeSeq() `;` reduceOCL()
//  val lowerAll = rise.elevate.rules.lowering.specializeSeq() `;` reduceOCL()

  // define lowering strategy
  // topdown map - mapGlb
  def lowerGs =
    topDown(rise.elevate.rules.lowering.mapGlobal(0))

  def lowerGsGs =
    topDown(rise.elevate.rules.lowering.mapGlobal(0)) `;`
      topDown(rise.elevate.rules.lowering.mapGlobal(1))

  def lowerWrgLcl =
    topDown(rise.elevate.rules.lowering.mapWorkGroup(0)) `;`
      topDown(rise.elevate.rules.lowering.mapLocal(0))

  def lowerWrgWrgLclLcl =
    topDown(rise.elevate.rules.lowering.mapWorkGroup(0)) `;`
      topDown(rise.elevate.rules.lowering.mapWorkGroup(1)) `;`
      topDown(rise.elevate.rules.lowering.mapLocal(0)) `;`
      topDown(rise.elevate.rules.lowering.mapLocal(1))

  // add strategies

  // id
  val idTd = topDown(idAfter)
  val idBu = bottomUp(idAfter)
  val idSome = some(idAfter)

  // split join
  val sjbu = bottomUp(tunable(splitJoin))(RiseTraversable)
  val sjtd = topDown(tunable(splitJoin))(RiseTraversable)

  // transpose
  val transposeIdSome = some(createTransposePair)
  val transposeIdRevert = some(removeTransposePair)

  val transposeIdBu = bottomUp(createTransposePair)
  val transposeIdTp = topDown(createTransposePair)

  val transposeIdRevertBu = bottomUp(removeTransposePair)
  val transposeIdRevertTp = topDown(removeTransposePair)

  val transposePairAfterTd = topDown(transposePairAfter)
  val transposePairAfterBu = bottomUp(transposePairAfter)
  val transposePairAfterSome = some(transposePairAfter)

  // rules
  val rules = Set(
    idAfter,
    tunable(splitJoin),
    createTransposePair,
    transposePairAfter,
    removeTransposePair,
    reduceMapFusion,
    reduceMapFission(),
    mapFusion,
    mapLastFission()
  )

  // idafter
  // splitjoin
  // transposePairAfter
  // create TransposePair
  // remove TransposePair

  // traversals
  val traversals = Set(
    all,
    one,
    some,
    oneUsingState,
    topDown,
    allTopdown,
    tryAll,
    allBottomup,
    downup,
    downup2,
    alltd,
    sometd,
    somebu
  )


  @rule def allSplitJoin:Strategy[Rise] = all(tunable(splitJoin))
  @rule def oneSplitJoin:Strategy[Rise] = one(tunable(splitJoin))
  @rule def someSplitJoin:Strategy[Rise] = some(tunable(splitJoin))
  @rule def oneUsingStateSplitJoin:Strategy[Rise] = oneUsingState(tunable(splitJoin))
  @rule def topDownSplitJoin:Strategy[Rise] = topDown(tunable(splitJoin))
  @rule def allTopdownSplitJoin:Strategy[Rise] = allTopdown(tunable(splitJoin))
  @rule def tryAllSplitJoin:Strategy[Rise] = tryAll(tunable(splitJoin))
  @rule def allBottomupSplitJoin:Strategy[Rise] = allBottomup(tunable(splitJoin))
  @rule def downupSplitJoin:Strategy[Rise] = downup(tunable(splitJoin))
  @rule def bottomUpSplitJoin:Strategy[Rise] = bottomUp(tunable(splitJoin))
  @rule def alltdSplitJoin:Strategy[Rise] = alltd(tunable(splitJoin))
  @rule def sometdSplitJoin:Strategy[Rise] = sometd(tunable(splitJoin))
  @rule def somebuSplitJoin:Strategy[Rise] = somebu(tunable(splitJoin))

  @rule def allIdAfter:Strategy[Rise] = all(idAfter)
  @rule def oneIdAfter:Strategy[Rise] = one(idAfter)
  @rule def someIdAfter:Strategy[Rise] = some(idAfter)
  @rule def oneUsingStateIdAfter:Strategy[Rise] = oneUsingState(idAfter)
  @rule def topDownIdAfter:Strategy[Rise] = topDown(idAfter)
  @rule def allTopdownIdAfter:Strategy[Rise] = allTopdown(idAfter)
  @rule def tryAllIdAfter:Strategy[Rise] = tryAll(idAfter)
  @rule def allBottomupIdAfter:Strategy[Rise] = allBottomup(idAfter)
  @rule def downupIdAfter:Strategy[Rise] = downup(idAfter)
  @rule def bottomUpIdAfter:Strategy[Rise] = bottomUp(idAfter)
  @rule def alltdIdAfter:Strategy[Rise] = alltd(idAfter)
  @rule def sometdIdAfter:Strategy[Rise] = sometd(idAfter)
  @rule def somebuIdAfter:Strategy[Rise] = somebu(idAfter)

  val strategies = Set[Strategy[Rise]](
    allIdAfter,
    oneIdAfter,
    someIdAfter,
    oneUsingStateIdAfter,
    topDownIdAfter,
    allTopdownIdAfter,
    tryAllIdAfter,
    allBottomupIdAfter,
    downupIdAfter,
    bottomUpIdAfter,
    alltdIdAfter,
    sometdIdAfter,
    somebuIdAfter,
    allSplitJoin,
    oneSplitJoin,
    someSplitJoin,
    oneUsingStateSplitJoin,
    topDownSplitJoin,
    allTopdownSplitJoin,
    tryAllSplitJoin,
    allBottomupSplitJoin,
    downupSplitJoin,
    bottomUpSplitJoin,
    alltdSplitJoin,
    sometdSplitJoin,
    somebuSplitJoin
  )

  def combine(rules: Set[Strategy[Rise]]): Set[Strategy[Rise]] = {
    val strategies = new ListBuffer[Strategy[Rise]]
    rules.foreach(rule => {
        strategies ++= ListBuffer(
        all(rule),
        one(rule),
        some(rule),
        oneUsingState(rule),
        topDown(rule),
        allTopdown(rule),
        tryAll(rule),
        allBottomup(rule),
        downup(rule),
        bottomUp(rule),
        alltd(rule),
        sometd(rule),
        somebu(rule)
      )
    })

    strategies.toSet
  }

  val strategies3 = combine(rules)


  println("strategies: ")
  strategies.foreach(println)


  val strategies2 = Set[Strategy[Rise]](
    //    idTd,
    //    idSome,
    //        createTransposePair,
    //        transposeIdSome,
    //        transposeIdRevert,
    //        transposeIdTp,
    //        transposeIdBu,
    //        transposeIdRevertBu,
    //        transposeIdRevertTp

    //    transposePairAfterTd,
    //      transposePairAfterSome

    sjtd,
    sjbu,
    idBu,
    transposePairAfterBu
  )


  // combine with leftchoice
  val lowering =
  fuseReduceMap2 `;`  // fuse Reduce and Map
      (
      lowerWrgWrgLclLcl <+ // try this lowering
        lowerWrgLcl <+  // else this
        lowerGsGs <+  // else this
        lowerGs // else this
      ) `;`
      lowerAll

  val lowerings = Set(
    lowerGs,
    lowerGsGs,
    lowerWrgLcl,
    lowerWrgWrgLclLcl
  )
}
