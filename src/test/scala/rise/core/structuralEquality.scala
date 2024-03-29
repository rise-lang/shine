package rise.core

import rise.core.DSL._
import rise.core.primitives._
import Type._
import org.junit.Assert.assertFalse
import rise.core.types._
import rise.core.types.DataType._
import shine.DPIA.Nat

class structuralEquality extends test_util.Tests {
  test("identity") {
    assert(fun(x => x).toUntypedExpr =~~= fun(y => y).toUntypedExpr)
  }

  test("reduce") {
    assert(
      depFun((n: Nat) =>
        fun(ArrayType(n, int))(a => reduceSeq(fun(x => fun(y => x + y)))(l(0))(a))
      ).toUntypedExpr
        =~~=
          depFun((m: Nat) =>
            fun(ArrayType(m, int))(b =>
              reduceSeq(fun(y => fun(x => y + x)))(l(0))(b)
            )
          ).toUntypedExpr
    )
  }

  test("reduce different init") {
    assertFalse(
      depFun((n: Nat) =>
        fun(ArrayType(n, int))(a => reduceSeq(fun(x => fun(y => x + y)))(l(0))(a))
      ).toUntypedExpr
        =~~=
          depFun((m: Nat) =>
            fun(ArrayType(m, int))(b =>
              reduceSeq(fun(y => fun(x => y + x)))(l(1))(b)
            )
          ).toUntypedExpr
    )
  }

  test("reduce different function structure") {
    assertFalse(
      depFun((n: Nat) =>
        fun(ArrayType(n, int))(a => reduceSeq(fun(x => fun(y => x + y)))(l(0))(a))
      ).toUntypedExpr
        =~~=
          depFun((m: Nat) =>
            fun(ArrayType(m, int))(b =>
              reduceSeq(fun(y => fun(x => x + y)))(l(0))(b)
            )
          ).toUntypedExpr
    )
  }

  test("reduce different type") {
    assertFalse(
      depFun((n: Nat) =>
        fun(ArrayType(n, int))(a => reduceSeq(fun(x => fun(y => x + y)))(l(0))(a))
      ).toUntypedExpr
        =~~=
          depFun((m: Nat) =>
            fun(ArrayType(m, f32))(b =>
              reduceSeq(fun(y => fun(x => y + x)))(l(0))(b)
            )
          ).toUntypedExpr
    )
  }

  test("map different implementations") {
    assertFalse(
      depFun((n: Nat) => fun(ArrayType(n, int))(a => map(fun(x => x))(a))).toUntypedExpr
        =~~=
          depFun((m: Nat) => fun(ArrayType(m, int))(b => mapSeq(fun(x => x))(b))).toUntypedExpr
    )
  }

  test("dependent function type using an array") {
    assert(
      expl((n: Nat) => expl((a: DataType) => expl((t: DataType) => ArrayType(n, a) ->: t)))
        =~=
          expl((m: Nat) => expl((b: DataType) => expl((t: DataType) => ArrayType(m, b) ->: t)))
    )
  }
}
