package rise.core

import rise.core.TypedDSL._
import rise.core.TypeLevelDSL._
import rise.core.types._

class typedDSL extends rise.test_util.Tests {
  test("Infer int addition type") {
    val e =
      nFun(n =>
        fun(
          DepArrayType(n, n2dtFun(i => (i + 1) `.` f32)) ->: DepArrayType(
            n,
            n2dtFun(i => (i + 1) `.` f32)
          )
        )(xs => xs |> depMapSeq(nFun(_ => mapSeq(fun(x => x)))))
      )
    val inferred: Expr = e
    println(inferred)
  }
}
