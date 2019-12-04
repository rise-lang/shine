package elevate.rise.strategies

import elevate.core.Strategy
import elevate.core.strategies.debug.peek
import elevate.rise.Rise
import lift.core.types.infer

object util {

  def inferType: Strategy[Rise] = peek(infer(_))

}
