package lift.core

import lift.core.DSL._
import lift.core.primitives._
import lift.core.semantics._
import lift.core.semantics.Conversions._
import lift.core.types._
import org.junit.Test

class depArrays extends idealised.util.Tests {

  val identity = nFun(n => fun(DepArrayType(n, i => ArrayType(i + 1, float)))(xs => xs))

  val add = fun(x => fun(a => x + a))


  val identityMap = nFun(n => fun(DepArrayType(n, i => ArrayType(i+1, int)))
    (xs => xs |> depMapSeq(nFun(_ => mapSeq(fun(x => x + 1))))))

  def check(e:Expr):Unit = {
    println(e)
    val typed = infer(e)
    println(typed)
  }

  @Test
  def test:Unit = {
    //check(identity)
    check(identityMap)
  }
}
