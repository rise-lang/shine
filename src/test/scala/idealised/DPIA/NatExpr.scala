package idealised.DPIA

import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker

class NatExpr extends idealised.util.Tests {
  test("Nat can be used as DataType inside of an expression in C.") {
    val simpleNatUsage = fun(NatType)(n => n + NatExpr(1))
    val program = idealised.C.ProgramGenerator.makeCode(TypeInference(simpleNatUsage, Map()).convertToPhrase)

    println(program.code)
    SyntaxChecker(program.code)
  }

  test("asNat acceptor translation is working correctly.") {
    val outputNat = fun(IndexType(4))(i => asNat(i))
    val program = idealised.C.ProgramGenerator.makeCode(TypeInference(outputNat, Map()).convertToPhrase)

    println(program.code)
    SyntaxChecker(program.code)
  }

  test("Type inference for AsNat fails if not passed value of IndexType.") {
    val notIndexType = fun(int)(i => asNat(i))

    assertThrows[TypeInferenceException] {
      TypeInference(notIndexType, Map())
    }
  }

  test("AsNat and plus operation generates syntactically correct code in C.") {
    val combiningIndexAndNat = nFun(n => fun(IndexType(n))(i => asNat(i) + NatExpr(n)))
    val program = idealised.C.ProgramGenerator.makeCode(TypeInference(combiningIndexAndNat, Map()).convertToPhrase)

    println(program.code)
    SyntaxChecker(program.code)
  }

  test("Nat is implicitly converted to NatExpr in an expression.") {
    val implCastCombinedIdxAndNat = nFun(n => fun(IndexType(n))(i => asNat(i) + n))
    val program = idealised.C.ProgramGenerator.makeCode(
      TypeInference(implCastCombinedIdxAndNat, Map()).convertToPhrase)

    println(program.code)
    SyntaxChecker(program.code)
  }

  test("unsafeAsIndex acceptor translation is working correctly.") {
    //TODO What about n? This generates a parameter for n although n does not appear in the generated code.
    val simpleNatToIndexUsage = nFun(n => fun(NatType)(i => unsafeAsIndex(n, i)))
    val program = idealised.C.ProgramGenerator.makeCode(TypeInference(simpleNatToIndexUsage, Map()).convertToPhrase)

    println(program.code)
    SyntaxChecker(program.code)
  }

  test("treatNatExprAsNat can simplify two NatExpr.") {
  //  val resNatExp = treatNatExprAsNat
  }
}
