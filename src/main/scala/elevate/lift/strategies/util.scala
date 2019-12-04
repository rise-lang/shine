package elevate.lift.strategies

import elevate.core.Strategy
import elevate.core.strategies.debug.peek
import elevate.lift.Lift
import lift.core.types.infer

object util {

  def inferType: Strategy[Lift] = peek(infer(_))

}
