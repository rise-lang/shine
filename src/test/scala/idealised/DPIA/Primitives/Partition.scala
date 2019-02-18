package idealised.DPIA.Primitives

import idealised.DPIA.NatIdentifier
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker
import lift.arithmetic._


class Partition extends idealised.util.Tests {
  test("Simple partition into a triangle C") {
    val N = SizeVar("N")
    val lenF = (i:NatIdentifier) => i + 1

    val slideExample = fun(ArrayType(N, float))(xs => xs :>> partition(3, lenF) :>> depMapSeq(mapSeq(fun(x => x))))

    val p = idealised.C.ProgramGenerator.makeCode(TypeInference(slideExample, Map()).toPhrase)
    val code = p.code
    SyntaxChecker(code)
    println(code)
  }

  test("Partition threeway with pad") {
    val N = SizeVar("N")

    val lenF =  SteppedCase(3, N, 3) _

    val padAndPartition = fun(ArrayType(N, float))(xs => xs :>>
      pad(3, 3,0.0f) :>>
      partition(3, lenF) :>> depMapSeq(fun(x => x)))

    val p = idealised.C.ProgramGenerator.makeCode(TypeInference(padAndPartition, Map()).toPhrase)
    val code = p.code
    SyntaxChecker(code)
    println(code)
  }
}
