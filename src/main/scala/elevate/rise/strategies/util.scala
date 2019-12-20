package elevate.rise.strategies

import elevate.core.Strategy
import elevate.core.strategies.debug.peek
import elevate.rise.Rise
import rise.core.types.infer

// todo remove as soon as all rules are type-preserving
object util {
  def inferType: Strategy[Rise] = peek(infer(_))
}
