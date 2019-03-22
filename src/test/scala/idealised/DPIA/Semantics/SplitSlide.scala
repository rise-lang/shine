package idealised.DPIA.Semantics

import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker

class SplitSlide extends idealised.util.Tests {

  val n = 8
  val sz = 3
  val sp = 1

  test("slide-split") {
    val slideExample = fun(ArrayType(130, float))(xs => xs :>> slide(sz, sp) :>> printType() :>> split(n) :>> printType() :>> mapSeq(mapSeq(mapSeq(fun(x => x)))) )

    val p = idealised.C.ProgramGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(slideExample, Map())))
    val code = p.code
    SyntaxChecker(code)
    println(code)

    "for".r.findAllIn(code).length shouldBe 3
  }

  test("slide-map(slide)") {
    val slideExample = fun(ArrayType(130, float))(xs => xs :>> slide(n+sz-sp, n) :>> printType() :>> map(slide(sz, sp)) :>> printType() :>> mapSeq(mapSeq(mapSeq(fun(x => x)))) )

    val p = idealised.C.ProgramGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(slideExample, Map())))
    val code = p.code
    SyntaxChecker(code)
    println(code)

    "for".r.findAllIn(code).length shouldBe 3
  }
}
