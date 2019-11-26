package lift.core

import lift.core.primitives._
import apps.convolution._

class showLift extends test_util.Tests {
  test("show blurXTiled2D as an example") {
    val probe: Expr => Boolean = {
      case _: PadClamp => true
      case _ => false
    }
    val example = blurXTiled2D
    println(example)
  }
}
