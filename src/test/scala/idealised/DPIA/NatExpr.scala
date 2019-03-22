package idealised.DPIA

import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker

class NatExpr extends idealised.util.Tests {
  test("Nat can be used as DataType inside of an expression in C.") {
    val simpleNatUsage = fun(NatType)(n => n + NatExpr(1))
    val program = idealised.C.ProgramGenerator.makeCode(
      idealised.DPIA.FromSurfaceLanguage(TypeInference(simpleNatUsage, Map())))

    println(program.code)
    SyntaxChecker(program.code)
  }

  test("asNat acceptor translation is working correctly.") {
    val outputNat = fun(IndexType(4))(i => indexAsNat(i))
    val program = idealised.C.ProgramGenerator.makeCode(
      idealised.DPIA.FromSurfaceLanguage(TypeInference(outputNat, Map())))

    println(program.code)
    SyntaxChecker(program.code)
  }

  test("Type inference for AsNat fails if not passed value of IndexType.") {
    val notIndexType = fun(int)(i => indexAsNat(i))

    assertThrows[TypeInferenceException] {
      TypeInference(notIndexType, Map())
    }
  }

  test("AsNat and plus operation generates syntactically correct code in C.") {
    val combiningIndexAndNat = nFun(n => fun(IndexType(n))(i => indexAsNat(i) + NatExpr(n)))
    val program = idealised.C.ProgramGenerator.makeCode(
      idealised.DPIA.FromSurfaceLanguage(TypeInference(combiningIndexAndNat, Map())))

    println(program.code)
    SyntaxChecker(program.code)
  }

  test("Nat is implicitly converted to NatExpr in an expression.") {
    val implCastCombinedIdxAndNat = nFun(n => fun(IndexType(n))(i => indexAsNat(i) + n))
    val program = idealised.C.ProgramGenerator.makeCode(
      idealised.DPIA.FromSurfaceLanguage(TypeInference(implCastCombinedIdxAndNat, Map())))

    println(program.code)
    SyntaxChecker(program.code)
  }

  test("asIndex acceptor translation is working correctly.") {
    val simpleNatToIndexUsage = nFun(n => fun(NatType)(i => asIndex(n, i)))
    val program = idealised.C.ProgramGenerator.makeCode(
      idealised.DPIA.FromSurfaceLanguage(TypeInference(simpleNatToIndexUsage, Map())))

    println(program.code)
    SyntaxChecker(program.code)
  }

  test("fmapNatExpr allows the combination of two NatExpr into one.") {
    val combined = mapNatExpr(NatExpr(8), NatExpr(2), (eight, two) => eight + two)
    assert(combined.n == lift.arithmetic.Cst(10))
  }
}
