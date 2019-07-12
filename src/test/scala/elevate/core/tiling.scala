package elevate.core

import elevate.core.strategies.liftTraversal._
import elevate.core.strategies.tiling._
import elevate.core.strategies._
import lift.core.DSL._
import lift.core._
import lift.core.primitives._

class tiling extends idealised.util.Tests {

  // notation
  def T: Expr = transpose
  def S: Expr = split(4)//slide(3)(1)
  def J: Expr = join
  def *(x: Expr): Expr = map(x)
  def **(x: Expr): Expr = map(map(x))
  def ***(x: Expr): Expr = map(map(map(x)))
  def ****(x: Expr): Expr = map(map(map(map(x))))
  def *****(x: Expr): Expr = map(map(map(map(map(x)))))
  def ******(x: Expr): Expr = map(map(map(map(map(map(x))))))
  def λ(f: Identifier => Expr): Expr = fun(f)

  test("simple 2d tiling") {
    val input = λ(f => **(f))

    println("testexpr: " + λ(f => **(f) >> T))
    println
    println(
      tile(32)(input)
    )
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
