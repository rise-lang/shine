package shine.DPIA

import arithexpr.arithmetic.Cst
import rise.core.DSL._
import rise.core.primitives._
import rise.core.types.DataType._
import util.gen
import util.gen.c.function

class NatExpr extends test_util.Tests {
  test("Nat can be used as DataType inside of an expression in C.") {
    function.asStringFromExpr(fun(NatType)(n => n + l(Cst(1))))
  }

  test("asNat acceptor translation is working correctly") {
    function.asStringFromExpr(fun(IndexType(4))(i => indexAsNat(i)))
  }

  test("AsNat and plus operation generates syntactically correct code in C.") {
    function.asStringFromExpr(depFun((n: Nat) => fun(IndexType(n))(i => indexAsNat(i) + n)))
  }

  test("Nat is implicitly converted to NatExpr in an expression.") {
    function.asStringFromExpr(depFun((n: Nat) => fun(IndexType(n))(i => indexAsNat(i) + n)))
  }

  test("asIndex acceptor translation is working correctly.") {
    function.asStringFromExpr(depFun((n: Nat) => fun(NatType)(i => natAsIndex(n)(i))))
  }
}
