package idealised.DPIA
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker

class DependentLet extends idealised.util.Tests{
  test("basic dlet test") {
    val f = fun(IndexType(10))(idx =>
      dlet(idx, lenF => fun(ArrayType(NatFunCall(lenF, Seq()), float))(xs =>
        xs :>> mapSeq(fun(x => x + 1.0f))
    )))

    val typed = TypeInference(f, Map())

    val p = idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(typed))

    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)
  }
}
