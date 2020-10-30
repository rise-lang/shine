package rise.core

import rise.elevate.util._
import rise.core.TypedDSL._
import rise.core.primitives._
import rise.core.TypeLevelDSL._
import rise.core.types.{Nat, f32}

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
    println(dotPrinter(λ(i => λ(f => *(f) $ i))))
  }

  test("typed *f") {
    println(dotPrinter(λ(i => λ(f => *(f) $ i))))
  }

  test("tiled 4D") {
    println(dotPrinter(λ(i => λ(f => (
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
            zip(transpose(b))(ac._2) |> map(fun(bc =>
              zip(ac._1)(bc._1) |>
                reduceSeq(fun( (acc, y) => acc + (y._1 * y._2)))(l(0.0f)) |>
                fun(x => (x * alpha) + (beta * bc._2))
            ))
          ))
        )
      )

    // without type inference
    println(dotPrinter(gemm.toUntypedExpr))
    // with type inference
    println(dotPrinter(gemm.toExpr))
  }
}
