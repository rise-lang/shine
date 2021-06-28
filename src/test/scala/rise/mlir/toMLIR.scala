package rise.mlir

import rise.core.types._
import rise.core.DSL._
import rise.core.primitives._
import rise.core.toMLIR

class toMLIR extends test_util.Tests {
  test("print simple zip Rise program as MLIR Cpp builder calls ") {
    val expr: rise.core.Expr = fun(ArrayType(1024, f32))(a => fun(ArrayType(1024, f32))(b =>
      zip(a)(b)
    ))

    println(expr)

    println(toMLIR.toCppBuilderAPI(expr))
  }

  test("print simple zip |> map Rise program as MLIR Cpp builder calls ") {
    val add = fun(x => fst(x) * snd(x))
    val expr: rise.core.Expr = fun(ArrayType(1024, f32))(a => fun(ArrayType(1024, f32))(b =>
      zip(a)(b) |> map(add)
    ))

    println(expr)

    println(toMLIR.toCppBuilderAPI(expr))
  }

  ignore("print simple zip |> map |> reduce Rise program as MLIR Cpp builder calls ") {
    val mult = fun(x => fst(x) * snd(x))
    val add = fun(x => fun(y => x + y))
    val expr: rise.core.Expr = fun(ArrayType(1024, f32))(a => fun(ArrayType(1024, f32))(b =>
      zip(a)(b) |> map(mult) |> reduceSeq(add)(lf32(0.0f))
    ))

    println(expr)

    println(toMLIR.toCppBuilderAPI(expr))
  }
}
