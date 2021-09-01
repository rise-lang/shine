package exploration.strategies
import elevate.core.Strategy
import elevate.core.strategies.traversal.{bottomUp, somebu, topDown}
import rise.elevate.{Rise, tunable}
import rise.elevate.rules.algorithmic.splitJoin
import rise.elevate.rules.lowering.addRequiredCopies
import rise.elevate.rules.traversal.default.RiseTraversable

object scalStrategies {

  // define lowering strategy
  // topdown map - mapGlb
  def lowerGS =
    topDown(rise.elevate.rules.lowering.mapGlobal(0)) `;` addRequiredCopies() `;` rise.elevate.rules.lowering.specializeSeq()

  // add strategies
  val sjtp = bottomUp(tunable(splitJoin))(RiseTraversable)

  val strategies = Set[Strategy[Rise]](
    sjtp
  )

  val lowering = lowerGS
}
