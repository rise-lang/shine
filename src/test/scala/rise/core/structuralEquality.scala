package rise.core

import rise.core.DSL._
import rise.core.TypeLevelDSL._
import rise.core.types._
import rise.core.primitives._

class structuralEquality extends test_util.Tests {
  test("identity") {
    assert(fun(x => x) == fun(y => y))
  }

  test("reduce") {
    assert(
      nFun(n => fun(ArrayType(n, int))(a =>
        reduceSeq(fun(x => fun(y => x + y)))(0)(a)))
        ==
      nFun(m => fun(ArrayType(m, int))(b =>
        reduceSeq(fun(y => fun(x => y + x)))(0)(b)))
    )
  }

  test("reduce different init") {
    assert(
      nFun(n => fun(ArrayType(n, int))(a =>
        reduceSeq(fun(x => fun(y => x + y)))(0)(a)))
        !=
      nFun(m => fun(ArrayType(m, int))(b =>
        reduceSeq(fun(y => fun(x => y + x)))(1)(b)))
    )
  }

  test("reduce different function structure") {
    assert(
      nFun(n => fun(ArrayType(n, int))(a =>
        reduceSeq(fun(x => fun(y => x + y)))(0)(a)))
        !=
      nFun(m => fun(ArrayType(m, int))(b =>
        reduceSeq(fun(y => fun(x => x + y)))(0)(b)))
    )
  }

  test("reduce different type") {
    assert(
      nFun(n => fun(ArrayType(n, int))(a =>
        reduceSeq(fun(x => fun(y => x + y)))(0)(a)))
        !=
      nFun(m => fun(ArrayType(m, float))(b =>
        reduceSeq(fun(y => fun(x => y + x)))(0)(b)))
    )
  }

  test("map different implementations") {
    assert(
      nFun(n => fun(ArrayType(n, int))(a => map(fun(x => x))(a)))
        !=
      nFun(m => fun(ArrayType(m, int))(b => mapSeq(fun(x => x))(b)))
    )
  }

  test("dependent function type using an array") {
    assert(
      nFunT(n => dtFunT(a => dtFunT(t => ArrayType(n, a) ->: t)))
        ==
      nFunT(m => dtFunT(b => dtFunT(t => ArrayType(m, b) ->: t)))
    )
  }
}