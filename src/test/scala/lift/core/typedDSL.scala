package lift.core

import lift.core.TypedDSL._
import lift.core.TypeLevelDSL._
import lift.core.types._

class typedDSL extends test_util.Tests {
  test("Infer int addition type") {
    val e =
      nFun(n => fun(DepArrayType(n, n2dtFun(i => (i+1)`.`float)) ->: DepArrayType(n, n2dtFun(i => (i+1)`.`float)))(xs =>
        xs |> depMapSeq(nFun(_ => mapSeq(fun(x => x))))))
    val infered: Expr = e
    println(infered)
  }
}
