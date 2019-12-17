package lift.core

import lift.core.DSL._
import lift.core.TypeLevelDSL._
import lift.core.types._

class infer extends test_util.Tests {
  test("Infer int addition type") {
    val typed = infer(l(1) + l(2))
    assert(typed.t == int)
  }

  test("Infer partial int addition type") {
    val typed = infer(fun(x => l(1) + x))
    assert(typed.t == int ->: int)
    val backward: Expr =
      infer(nFun((m, n, k) =>
        fun((m`.`k`.`float) ->: (k`.`n`.`float) ->: (m`.`n`.`float) ->: float ->: float ->: (n`.`m`.`float))
        ((a, b, c, alpha, beta) =>
          (transpose o map(fun(ac =>
            map(fun(bc =>
              (fun(x => (x * alpha) + beta * bc._2) o
                reduceSeq(fun((acc, y) => acc + (y._1 * y._2)), l(0.0f))) $
                zip(ac._1, bc._1))) $
              zip(transpose(b),ac._2)))) $
            zip(a, c)
        )
      ))
  }
}
