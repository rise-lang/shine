package rise.core

import rise.core.TypedDSL._
import rise.core.primitives._
import rise.core.TypeLevelDSL._
import rise.core.types._
import shine.DPIA.Nat

class structuralEquality extends test_util.Tests {
  test("identity") {
    assert(fun(x => x).toUntypedExpr == fun(y => y).toUntypedExpr)
  }

  test("reduce") {
    assert(
      depFun((n: Nat) =>
        fun(ArrayType(n, int))(a => reduceSeq(fun(x => fun(y => x + y)))(0)(a))
      ).toUntypedExpr
        ==
          depFun((m: Nat) =>
            fun(ArrayType(m, int))(b =>
              reduceSeq(fun(y => fun(x => y + x)))(0)(b)
            )
          ).toUntypedExpr
    )
  }

  test("reduce different init") {
    assert(
      depFun((n: Nat) =>
        fun(ArrayType(n, int))(a => reduceSeq(fun(x => fun(y => x + y)))(0)(a))
      ).toUntypedExpr
        !=
          depFun((m: Nat) =>
            fun(ArrayType(m, int))(b =>
              reduceSeq(fun(y => fun(x => y + x)))(1)(b)
            )
          ).toUntypedExpr
    )
  }

  test("reduce different function structure") {
    assert(
      depFun((n: Nat) =>
        fun(ArrayType(n, int))(a => reduceSeq(fun(x => fun(y => x + y)))(0)(a))
      ).toUntypedExpr
        !=
          depFun((m: Nat) =>
            fun(ArrayType(m, int))(b =>
              reduceSeq(fun(y => fun(x => x + y)))(0)(b)
            )
          ).toUntypedExpr
    )
  }

  // TODO: implement equality properly
  ignore("reduce different type") {
    assert(
      depFun((n: Nat) =>
        fun(ArrayType(n, int))(a => reduceSeq(fun(x => fun(y => x + y)))(0)(a))
      )
        !=
          depFun((m: Nat) =>
            fun(ArrayType(m, f32))(b =>
              reduceSeq(fun(y => fun(x => y + x)))(0)(b)
            )
          )
    )
  }

  test("map different implementations") {
    assert(
      depFun((n: Nat) => fun(ArrayType(n, int))(a => map(fun(x => x))(a))).toUntypedExpr
        !=
          depFun((m: Nat) => fun(ArrayType(m, int))(b => mapSeq(fun(x => x))(b))).toUntypedExpr
    )
  }

  test("dependent function type using an array") {
    assert(
      expl((n: Nat) => expl((a: DataType) => expl((t: DataType) => ArrayType(n, a) ->: t)))
        ==
          expl((m: Nat) => expl((b: DataType) => expl((t: DataType) => ArrayType(m, b) ->: t)))
    )
  }
}
