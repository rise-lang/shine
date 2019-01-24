package idealised.DPIA.Primitives

import lift.arithmetic._
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker

class Take extends idealised.util.Tests {
  test("Trigger TakeAcc acceptor translation, what should happen?") {
    val e = fun(ArrayType(128, int))(a => take(8, a))

    val p = idealised.C.ProgramGenerator.makeCode(TypeInference(e, Map()).toPhrase)
    val code = p.code
    SyntaxChecker(code)
    println(code)
  }
}
