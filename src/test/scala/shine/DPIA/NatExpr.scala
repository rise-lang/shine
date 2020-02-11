package shine.DPIA

import rise.core._
import rise.core.DSL._
import rise.core.types._
import rise.core.semantics.NatData
import util.gen

class NatExpr extends shine.test_util.Tests {
  test("Nat can be used as DataType inside of an expression in C.") {
    gen.CProgram(fun(NatType)(n => n + Literal(NatData(1))))
  }

  test("asNat acceptor translation is working correctly") {
    gen.CProgram(fun(IndexType(4))(i => indexAsNat(i)))
  }

  test("AsNat and plus operation generates syntactically correct code in C.") {
    gen.CProgram(nFun(n => fun(IndexType(n))(i => indexAsNat(i) + n)))
  }

  test("Nat is implicitly converted to NatExpr in an expression.") {
    gen.CProgram(nFun(n => fun(IndexType(n))(i => indexAsNat(i) + n)))
  }

  test("asIndex acceptor translation is working correctly.") {
    gen.CProgram(nFun(n => fun(NatType)(i => natAsIndex(n)(i))))
  }
}
