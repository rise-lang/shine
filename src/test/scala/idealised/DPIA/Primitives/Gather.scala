package idealised.DPIA.Primitives

import idealised.OpenCL.SurfaceLanguage.DSL.reorderWithStridePhrase
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker
import lift.arithmetic._

class Gather extends idealised.util.Tests {

  test("Simple scatter example should generate syntactic valid C code with two one loops") {
    val slideExample = fun(ArrayType(SizeVar("N"), float))(xs => xs :>> gather(reorderWithStridePhrase(128)) :>> mapSeq(fun(x => x)) )

    val p = idealised.C.ProgramGenerator.makeCode(TypeInference(slideExample, Map()).toPhrase)
    val code = p.code
    SyntaxChecker(code)
    println(code)

    "for".r.findAllIn(code).length shouldBe 1
  }

  test("Simple 2D scatter example should generate syntactic valid C code with two two loops") {
    val slideExample = fun(ArrayType(SizeVar("N"), ArrayType(SizeVar("M"), float)))(xs =>
      xs :>> map(gather(reorderWithStridePhrase(128))) :>> mapSeq(mapSeq(fun(x => x))) )

    val p = idealised.C.ProgramGenerator.makeCode(TypeInference(slideExample, Map()).toPhrase)
    val code = p.code
    SyntaxChecker(code)
    println(code)

    "for".r.findAllIn(code).length shouldBe 2
  }

}
