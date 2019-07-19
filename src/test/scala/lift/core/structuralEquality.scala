package lift.core

import lift.core.DSL._
import lift.core.types._
import lift.core.primitives._

class structuralEquality extends idealised.util.Tests {
  test("identity") {
    assert(StructuralEquality(fun(x => x), fun(y => y)))
  }

  test("reduce") {
    assert(StructuralEquality(
      nFun(n => fun(ArrayType(n, int))(a =>
        reduceSeq(fun(x => fun(y => x + y)))(0)(a))),
      nFun(m => fun(ArrayType(m, int))(b =>
        reduceSeq(fun(y => fun(x => y + x)))(0)(b)))
    ))
  }

  test("reduce different init") {
    assert(!StructuralEquality(
      nFun(n => fun(ArrayType(n, int))(a =>
        reduceSeq(fun(x => fun(y => x + y)))(0)(a))),
      nFun(m => fun(ArrayType(m, int))(b =>
        reduceSeq(fun(y => fun(x => y + x)))(1)(b)))
    ))
  }

  test("reduce different function structure") {
    assert(!StructuralEquality(
      nFun(n => fun(ArrayType(n, int))(a =>
        reduceSeq(fun(x => fun(y => x + y)))(0)(a))),
      nFun(m => fun(ArrayType(m, int))(b =>
        reduceSeq(fun(y => fun(x => x + y)))(0)(b)))
    ))
  }

  test("reduce different type") {
    assert(!StructuralEquality(
      nFun(n => fun(ArrayType(n, int))(a =>
        reduceSeq(fun(x => fun(y => x + y)))(0)(a))),
      nFun(m => fun(ArrayType(m, float))(b =>
        reduceSeq(fun(y => fun(x => y + x)))(0)(b)))
    ))
  }

  test("map different implementations") {
    assert(!StructuralEquality(
      nFun(n => fun(ArrayType(n, int))(a => map(fun(x => x))(a))),
      nFun(m => fun(ArrayType(m, int))(b => mapSeq(fun(x => x))(b)))
    ))
  }

  test("dependent function type using an array") {
    assert(StructuralEquality(
      nFunT(n => dtFunT(a => dtFunT(t => ArrayType(n, a) ->: t))),
      nFunT(m => dtFunT(b => dtFunT(t => ArrayType(m, b) ->: t)))
    ))
  }
}