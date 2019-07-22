package elevate.core

import elevate.lift.strategies.traversal._
import elevate.lift.strategies.normalForm._
import elevate.core.strategies.tiling._
import elevate.core.strategies.basic._
import elevate.lift._
import elevate.lift.rules._
import lift.core.DSL._
import lift.core.primitives._
import lift.core.types.infer


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
    /*
    assert(structEq(
      body(body(loopInterchange))(λ(in => λ(f => **(f) <| in))).get,
      λ(in => λ(f => transpose << **(f) << transpose <| in))))
     */

    //println(λ(in => λ(f => ***(f) $ in)))
    //println(reductionNormalForm(λ(in => λ(f => *(transpose o transpose o **(f)) $ in))).get)
    //println(λ(in => λ(f => *(transpose >> **(f) >> transpose) $ in)))

    println(body(body(
      function(argument(
      loopInterchange))))(λ(in => λ(f => ***(f) $ in))))

    println("----------")
    assert(structEq(
      body(body(print `;` fmap(loopInterchange `;` rewriteNormalForm)))(λ(in => λ(f => ***(f) $ in))).get,
      λ(in => λ(f => (transpose o **(f) o transpose) $ in))))
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
