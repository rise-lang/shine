package exploration

import rise.core.TypedDSL.{fun, map}

//test to run exploration
class explore extends shine.test_util.Tests {
  println("start test exploration")

  //simple example expression
  val s = fun(f => fun(g => map(f) >> map(g)))

  riseExploration.main(s)

  println("end test exploration")
}
