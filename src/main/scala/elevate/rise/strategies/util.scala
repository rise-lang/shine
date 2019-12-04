package elevate.rise.strategies

import elevate.core.Strategy
import elevate.core.strategies.debug.peek
import elevate.rise.Lift
import lift.core.types.infer

object util {

  def inferType: Strategy[Lift] = peek(infer(_))

}
