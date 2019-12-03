package elevate.lift.strategies

import elevate.core.strategies.basic.peek
import elevate.core.Strategy
import elevate.lift.Lift
import lift.core.types.infer

object util {

  def inferType: Strategy[Lift] = peek(infer(_))

}
