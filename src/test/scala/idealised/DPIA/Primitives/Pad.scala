package idealised.DPIA.Primitives

import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Primitives.Pad
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker
import lift.arithmetic._

class Pad extends idealised.util.Tests {
  test("Pad in C then copy") {
    val f = fun(ArrayType(SizeVar("N"), float))( xs =>
      mapSeq(fun(x => x + 1.0f), Pad(2, 3, 5.0f, xs, None))
    )

    val p = idealised.C.ProgramGenerator.makeCode(TypeInference(f, Map()).toPhrase)
    val code = p.code
    SyntaxChecker(code)
    println(code)
  }
}
