package elevate.core

import elevate.lift.strategies.traversal._
import elevate.core.strategies.tiling._
import elevate.lift._
import lift.core.DSL._
import lift.core.primitives._


class tiling extends idealised.util.Tests {

  test("simple 2d tiling") {
    val input = λ(f => **(f))

    println("testexpr: " + λ(f => **(f) >> T))
    println
    println(
      tile(32)(input)
    )
  }

  test("loop interchange") {
    assert(structEq(
      body(body(loopInterchange))(λ(in => λ(f => **(f) <| in))).get,
      λ(in => λ(f => transpose << **(f) << transpose <| in))))
  }

  test("tiling0") {
    println("tiling1")
    println(body(tiling1)(λ(f => *(f))))
    println
    println("tiling2")
    println(body(tiling2(tiling1))(λ(f => **(f))))
    println
    println("tiling3")
    println(body(tiling2(tiling2(tiling1)))(λ(f => ***(f))))
  }
}
