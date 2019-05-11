package lift.core

import lift.core.DSL._
import lift.core.types._
import org.junit.Test

class depArrays extends idealised.util.Tests {

  val identity = nFun(n => fun(DepArrayType(n, natTypeFun(i => ArrayType(i + 1, float))))(xs => xs))

  def check(e:Expr):Unit = {
    println(e)
    val typed = infer(e)
    println(typed)
  }

  @Test
  def test:Unit = {
    check(identity)
  }
}
