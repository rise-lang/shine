package elevate.core.strategies

import elevate.core.Strategy

trait Traversable[P] {
  def all: Strategy[P] => Strategy[P]
}
