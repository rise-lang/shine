package shine.DPIA

import arithexpr.arithmetic.Cst
import rise.core.dsl._
import rise.core.exprs.primitives._
import rise.core.types._
import rise.core.util.gen

class NatExpr extends test_util.Tests {
  test("Nat can be used as DataType inside of an expression in C.") {
    gen.CProgram(fun(NatType)(n => n + l(Cst(1))))
  }

  test("asNat acceptor translation is working correctly") {
    gen.CProgram(fun(IndexType(4))(i => indexAsNat(i)))
  }

  test("AsNat and plus operation generates syntactically correct code in C.") {
    gen.CProgram(depFun((n: Nat) => fun(IndexType(n))(i => indexAsNat(i) + n)))
  }

  test("Nat is implicitly converted to NatExpr in an expression.") {
    gen.CProgram(depFun((n: Nat) => fun(IndexType(n))(i => indexAsNat(i) + n)))
  }

  test("asIndex acceptor translation is working correctly.") {
    gen.CProgram(depFun((n: Nat) => fun(NatType)(i => natAsIndex(n)(i))))
  }
}
