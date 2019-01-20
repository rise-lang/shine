package idealised.DPIA.Primitives

import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker
import lift.arithmetic._

class Map extends idealised.util.Tests {

  test("Simple 1D map example should generate syntactic valid C code with one for loop") {
    val slideExample = fun(ArrayType(SizeVar("N"), float))(xs => xs :>> mapSeq(fun(x => x)))

    val p = idealised.C.ProgramGenerator.makeCode(TypeInference(slideExample, Map()).toPhrase)
    val code = p.code
    SyntaxChecker(code)
    println(code)

    "for".r.findAllIn(code).length shouldBe 1
  }

  test("Simple 2D map example should generate syntactic valid C code with two for loop") {
    val slideExample = fun(ArrayType(SizeVar("N"), ArrayType(SizeVar("M"), float)))(xs => xs :>> mapSeq(mapSeq(fun(x => x))))

    val p = idealised.C.ProgramGenerator.makeCode(TypeInference(slideExample, Map()).toPhrase)
    val code = p.code
    SyntaxChecker(code)
    println(code)

    "for".r.findAllIn(code).length shouldBe 2
  }

  test("Simple 3D map example should generate syntactic valid C code with three for loop") {
    val slideExample = fun(ArrayType(SizeVar("N"), ArrayType(SizeVar("M"), ArrayType(SizeVar("O"), float))))(xs =>
      xs :>> mapSeq(mapSeq(mapSeq(fun(x => x)))))

    val p = idealised.C.ProgramGenerator.makeCode(TypeInference(slideExample, Map()).toPhrase)
    val code = p.code
    SyntaxChecker(code)
    println(code)

    "for".r.findAllIn(code).length shouldBe 3
  }

}
