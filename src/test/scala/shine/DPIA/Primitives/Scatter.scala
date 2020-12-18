package shine.DPIA.Primitives

import rise.core.primitives._
import rise.core.DSL._
import rise.core.types._
import rise.core.DSL.Type._
import util.gen

import scala.language.postfixOps

class Scatter extends test_util.Tests {
  test("Reversing scatter should generate valid OpenCL") {
    import rise.openCL.TypedDSL._
    import shine.OpenCL._

    val N = 20
    val n: Nat = N
    val e = fun((n`.`int) ->: (n`.`int))(a => a |>
      mapGlobal(0)(fun(x => x)) |>
      scatter(generate(fun(i =>
        natAsIndex(n)(l(n - 1) - indexAsNat(i)))))
    )

    val k = gen.OpenCLKernel(e)
    val lS = LocalSize(1)
    val gS = GlobalSize(2)
    val f = k.as[ScalaFunction `(` Array[Int] `)=>` Array[Int]]
    val input = (1 to N).toArray
    val expected = input.reverse
    val (r, _) = util.withExecutor {
      f(lS, gS)(input `;`)
    }
    util.assertSame(r, expected, "unexpected result")
  }

  test("Overriding scatter should generate valid OpenCL") {
    import rise.openCL.TypedDSL._
    import shine.OpenCL._

    val N = 20
    val n: Nat = N
    val e = fun((2`.`n`.`int) ->: (n`.`int))(a => a |>
      mapGlobal(0)(mapSeq(fun(x => x))) |>
      join |>
      scatter(generate(fun(i => {
        val end = l(n - 1)
        natAsIndex(n)(end - indexAsNat(i) % n)
      })))
    )

    val k = gen.OpenCLKernel(e)
    val lS = LocalSize(1)
    val gS = GlobalSize(2)
    val f = k.as[ScalaFunction `(` Array[Int] `)=>` Array[Int]]
    val input = Array.fill(2)((1 to N).toArray)
    val expected = input(0).reverse
    val (r, _) = util.withExecutor {
      f(lS, gS)(input.flatten `;`)
    }
    util.assertSame(r, expected, "unexpected result")
  }
}
