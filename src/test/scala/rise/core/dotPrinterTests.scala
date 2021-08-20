package rise.core

import rise.elevate.util._
import rise.core.DSL._
import rise.core.primitives._
import Type._
import rise.core.types.Nat
import rise.core.types.DataType._

class dotPrinterTests extends test_util.Tests {

  def exprToDot(path: String, name: String, e: Expr, dot: Expr => String): Unit = {
    import java.io._
    import sys.process._

    val w =new PrintWriter(new File(s"$path/$name.dot"))
    w.write(dot(e))
    w.flush()
    w.close()
    s"dot -Tpdf $path/$name.dot -o $path/$name.pdf".!
  }

  test("simple *f") {
    logger.debug(dotPrinter(λ(i => λ(f => *(f) $ i))))
  }

  test("typed *f") {
    logger.debug(dotPrinter(λ(i => λ(f => *(f) $ i))))
  }

  test("tiled 4D") {
    logger.debug(dotPrinter(λ(i => λ(f => (
      J o **(J) o ****(J) o ******(J) o
        *****(T) o ***(T) o ****(T) o *(T) o **(T) o ***(T) o
        ****(****(f)) o
        ***(T) o **(T) o *(T) o ****(T) o ***(T) o *****(T) o
        ******(S) o ****(S) o **(S) o S) $ i
    ))))
  }

  test("gemm") {
    val gemm =
      depFun((n: Nat, m: Nat, k: Nat) =>
        fun((n`.`k`.`f32) ->: (k`.`m`.`f32) ->: (n`.`m`.`f32) ->: f32 ->: f32 ->: (n`.`m`.`f32))
        ((a, b, c, alpha, beta) =>

          zip(a)(c) |> map(fun(ac =>
            zip(transpose(b))(ac.`2`) |> map(fun(bc =>
              zip(ac.`1`)(bc.`1`) |>
                reduceSeq(fun( (acc, y) => acc + (y.`1` * y.`2`)))(lf32(0.0f)) |>
                fun(x => (x * alpha) + (beta * bc.`2`))
            ))
          ))
        )
      )

    // without type inference
    logger.debug(dotPrinter(gemm.toUntypedExpr))
    // with type inference
    logger.debug(dotPrinter(gemm.toExpr))
  }
}
