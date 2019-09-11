/* TODO
package idealised.DPIA
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker

class DependentArrays extends idealised.util.Tests {

  test("Simple depMapSeq test") {
    val f =
      nFun(n =>
        fun(DepArrayType(n, i =>
          ArrayType(i + 1, int)))(array => depMapSeq(fun(x => mapSeq(fun(y => y + 1), x) ), array)))

    val p = idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(f, Map())))

    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)
  }

  test("Nested dep arrays") {
    val splitExample =
      nFun(n =>
        fun(DepArrayType(n, i => DepArrayType(4, j => ArrayType(i + 1, float))))(xs =>
          xs :>> depMapSeq(fun(row => depMapSeq(fun(col => mapSeq(fun(x => x + 1.0f), col)), row)))
    ))

    val splitExampleTyped = idealised.DPIA.FromSurfaceLanguage(TypeInference(splitExample, Map()))
    println(splitExampleTyped)
    val p = idealised.OpenCL.KernelGenerator.makeCode(splitExampleTyped)
    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)
  }

  test("Big sum generates a for loop") {
    val f =
      nFun(n =>
        fun(DepArrayType(n, i =>
          ArrayType(i % 4, int)))(array => depMapSeq(fun(x => mapSeq(fun(y => y + 1), x) ), array)))

    val p = idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(f, Map())))

    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)

    "for".r.findAllIn(code).length shouldBe 4
  }
}
 */