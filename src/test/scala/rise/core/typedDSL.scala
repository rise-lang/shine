package rise.core

import rise.core.TypedDSL._
import rise.core.TypeLevelDSL._
import rise.core.types._

class typedDSL extends rise.testUtil.Tests {
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
    println(inferred.t)
  }

  test("Dependent pair construct") {
    val e = nFun(n =>
      fun(n `.` f32)(x => dpair(n)(x))
    )
     val inferred: Expr = e
    println(inferred)
    print(inferred.t)
  }

  test("Dependent pair match") {
    val e = fun(n2dPairT(n => n`.`f32))(pair =>
      dmatch(pair)(nFun(n => fun(x => dpair(n)(x))))
    )
    val inferred: Expr = e
    println(inferred)
    print(inferred.t)
  }

  test("Dependent pair match with reduction") {
    val e = fun(n2dPairT(n => n`.`f32))(pair =>
      dmatch(pair)(nFun(_ => fun(xs =>
        reduceSeq(fun(x => fun(y => x + y)))(l(1.0f))(xs))
      ))
    )
    val inferred: Expr = e
    println(inferred)
    print(inferred.t)
  }

  test("List of list matrix vector multiplication") {
    val e = nFun(n => fun(n `..` (i => (i+1) `.` f32))(array =>
        depMapSeq(nFun(_ => mapSeq(fun(x => x))))(array)
      ))

    val inferred: Expr = e
    println(inferred)
    print(inferred.t)
  }
}
