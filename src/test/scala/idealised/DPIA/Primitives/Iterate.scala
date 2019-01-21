package idealised.DPIA.Primitives

import lift.arithmetic._
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker

class Iterate extends idealised.util.Tests {
  test("Simple iterate example should generate syntactic valid C code ") {
    val e = fun(ArrayType(128, int))(a => a :>> iterate(6, split(2) >>> mapSeq(reduceSeq(fun(_ + _), 0))))

    val p = idealised.C.ProgramGenerator.makeCode(TypeInference(e, Map()).toPhrase)
    val code = p.code
    SyntaxChecker(code)
    println(code)
  }
}
