package lift.core

import lift.core.types._
import lift.core.primitives._
import lift.core.traversal._
import lift.core.DSL._
import org.junit.Test

class depArrays extends idealised.util.Tests {

  val identity = nFun(n => fun(DepArrayType(n, nTypeFun(i => ArrayType(i + 1, float))))(xs => xs))

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
