package exploration.strategies
import elevate.core.Strategy
import elevate.core.strategies.traversal.{allTopdown, bottomUp, somebu, topDown}
import rise.elevate.{Rise, tunable}
import rise.elevate.rules.algorithmic.{fuseReduceMap, splitJoin}
import rise.elevate.rules.lowering.{addRequiredCopies, reduceOCL}
import rise.elevate.rules.traversal.default.RiseTraversable

object scalStrategies {

  val lowerAll = addRequiredCopies() `;` rise.elevate.rules.lowering.specializeSeq() `;` reduceOCL()

  // define lowering strategy
  // topdown map - mapGlb
  def lowerGs = {
    topDown(rise.elevate.rules.lowering.mapGlobal(0)) `;`
      lowerAll
  }

  def lowerGsGs =
    topDown(rise.elevate.rules.lowering.mapGlobal(0)) `;`
      topDown(rise.elevate.rules.lowering.mapGlobal(1)) `;`
      lowerAll


  def lowerWrgLcl =
    topDown(rise.elevate.rules.lowering.mapWorkGroup(0)) `;`
      topDown(rise.elevate.rules.lowering.mapLocal(0)) `;`
      lowerAll

  def lowerWrgWrgLclLcl =
    topDown(rise.elevate.rules.lowering.mapWorkGroup(0)) `;`
      topDown(rise.elevate.rules.lowering.mapWorkGroup(1)) `;`
      topDown(rise.elevate.rules.lowering.mapLocal(0)) `;`
      topDown(rise.elevate.rules.lowering.mapLocal(1)) `;`
      lowerAll

  // add strategies
  val sjtp = bottomUp(tunable(splitJoin))(RiseTraversable)

  val strategies = Set[Strategy[Rise]](
    sjtp
  )


  // combine with leftchoice
  val lowering =
    allTopdown(fuseReduceMap) `;`  // fuse Reduce and Map
      lowerWrgWrgLclLcl <+ // try this lowering
        lowerWrgLcl <+  // else this
        lowerGsGs <+  // else this
        lowerGs // else this

  val lowerings = Set(
    lowerGs,
    lowerGsGs,
    lowerWrgLcl,
    lowerWrgWrgLclLcl
  )
}
