package idealised.DPIA
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker
import lift.arithmetic._

class DependentArrays extends idealised.util.Tests {

  test("Simple depMapSeq test") {
    val f = fun(DepArrayType(SizeVar("N"), i => ArrayType(i + 1, int)))(array => depMapSeq(fun(x => mapSeq(fun(y => y + 1), x) ), array))

    val p = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(f, Map()).toPhrase, ?, ?)

    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)
  }

  test("Nested dep arrays") {
    val splitExample = fun(DepArrayType(SizeVar("N"), i => DepArrayType(SizeVar("M"), j => ArrayType(i * 4 + j + 1, float))))(xs =>
      xs :>> depMapSeq(fun(row => depMapSeq(fun(col => mapSeq(fun(x => x + 1.0f), col)), row)))
    )

    val p = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(splitExample, Map()).toPhrase, ?, ?)
    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)
  }
}
