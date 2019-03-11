package idealised.DPIA

import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker

class NatExpr extends idealised.util.Tests {
  test("Nat can be used as DataType inside of an expression in C.") {
    val simpleGenerate = fun(NatType)(i => i + NatExpr(1))
    val program = idealised.C.ProgramGenerator.makeCode(TypeInference(simpleGenerate, Map()).convertToPhrase)

    println(program.code)
    SyntaxChecker(program.code)
  }

  test("Type inference for AsNat fails if not passed value of IndexType.") {
    val simpleGenerate = fun(int)(i => asNat(i))

    assertThrows[TypeInferenceException] {
      TypeInference(simpleGenerate, Map())
    }
  }

  test("AsNat and plus operation generates syntactically correct code in C.") {
    val simpleGenerate = nFun(n => fun(IndexType(n))(i => asNat(i) + NatExpr(n)))
    val program = idealised.C.ProgramGenerator.makeCode(TypeInference(simpleGenerate, Map()).convertToPhrase)

    println(program.code)
    SyntaxChecker(program.code)
  }

  test("Nat is implicitly converted to NatExpr in an expression.") {
    //TODO make asNat(i: IdentifierExpr) implicit? This would then fail for every IdentifierExpr not of type IndexType
    val simpleGenerate = nFun(n => fun(IndexType(n))(i => asNat(i) + n))
    val program = idealised.C.ProgramGenerator.makeCode(TypeInference(simpleGenerate, Map()).convertToPhrase)

    println(program.code)
    SyntaxChecker(program.code)
  }

}
