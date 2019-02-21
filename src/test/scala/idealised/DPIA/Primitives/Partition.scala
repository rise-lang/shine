package idealised.DPIA.Primitives

import idealised.DPIA.NatIdentifier
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker
import lift.arithmetic._


class Partition extends idealised.util.Tests {
  test("Simple partition into a triangle C") {
    val lenF = (i:NatIdentifier) => i + 1

    val slideExample =
      dFun((n: NatIdentifier) =>
        fun(ArrayType(n, float))(xs => xs :>> partition(3, lenF) :>> depMapSeq(mapSeq(fun(x => x)))))

    val p = idealised.C.ProgramGenerator.makeCode(TypeInference(slideExample, Map()).toPhrase)
    val code = p.code
    SyntaxChecker(code)
    println(code)
  }
}
