package idealised.DPIA
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.LiteralExpr
import idealised.SurfaceLanguage.Primitives.{AsIndex, Idx}
import idealised.SurfaceLanguage.Semantics.{IndexData, IntData}
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker

class LetNat extends idealised.util.Tests{

  test("Simple functions with no capture") {
    val program = nFun(n =>
      fun(ArrayType(n, float))(xs =>
        letNat(fun(int)(x => x + 2), f =>
          letNat(fun(int)(x => x + 1), g =>
            letNat(5, five =>
              mapSeq(fun(x => x))(take(g(f(five())), xs))
            )
          )
        )
    ))

    val typed = TypeInference(program, Map())

    val p = idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(typed))

    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)
  }

  test("Inlined single value") {
    val f = nFun(n => fun(IndexType(10))(idx =>
        fun(ArrayType(n, float))(xs =>
            letNat(idx,
              f => xs :>> take(f()) :>> mapSeq(fun(x => x + 1.0f)))
        )
      ))

    val typed = TypeInference(f, Map())

    val p = idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(typed))

    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)
  }

  test("Dictionary-dependent array") {
    val f = nFun(n => fun(ArrayType(n, int))(dict =>
      letNat(nFun(i => Idx(dict, asIndex(n, i))),
        length => fun(DepArrayType(n, i => ArrayType(length(i), float)))(xs => xs)
      )))

    val typed = TypeInference(f, Map())

    val p = idealised.OpenCL.KernelGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(typed))

    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)
  }
}
