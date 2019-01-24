package idealised.DPIA.Primitives

import idealised.C.Program
import lift.arithmetic._
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker

class MapSeqSlide extends idealised.util.Tests {
  test("Simple example should generate syntactic valid C code") {
    val e = fun(ArrayType(SizeVar("N"), float))(a =>
      a :>> mapSeqSlide(3, 1, reduceSeq(fun(_ + _), 0.0f)))
    val p = idealised.C.ProgramGenerator.makeCode(TypeInference(e, Map()).toPhrase)
    val code = p.code
    SyntaxChecker(code)
    println(code)
  }
}
