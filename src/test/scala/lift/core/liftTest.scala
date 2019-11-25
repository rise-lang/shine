package lift.core

import lift.core.DSL._
import lift.core.HighLevelConstructs._
import lift.core.primitives._
import lift.core.semantics.Data
import lift.core.types._
import apps.convolution._

class liftTest extends test_util.Tests {
  test("some random tests here") {
    val probe: Expr => Boolean = {
      case _: PadClamp => true
      case _ => false
    }
    val x = blurXTiled2D
    println(x)
    val y = blurYTiled2DTiledLoadingTransposed
    println(y)
  }
}
