package idealised.apps

import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker
import lift.arithmetic._

class asum extends idealised.util.Tests {

  val N = SizeVar("N")
  val inputT = ArrayType(N, float)
  val abs = (t: DataType) => fun(x => foreignFun(t, "abs", (t, "y"), "{ return fabs(y); }", x))
  val fabs = abs(float)
  val add = fun(x => fun(a => x + a))

  val high_level = fun(inputT)(input =>
    input :>> map(fabs) :>> reduceSeq(add, 0.0f) )

  test("High level asum type inference works") {
    val typed = TypeInference(high_level, Map())

    assertResult(FunctionType(inputT, float)) {
      typed.t.get
    }
  }

  test("High level asum compiles to syntactically correct C") {
    val p = idealised.C.ProgramGenerator.makeCode(TypeInference(high_level, Map()).toPhrase)
    SyntaxChecker(p.code)
    println(p.code)
  }

  test("Intel derived no warp compiles to syntactically correct OpenMP code") {
    import idealised.OpenMP.SurfaceLanguage.DSL._
    import idealised.OpenCL.SurfaceLanguage.DSL._

    val intelDerivedNoWarp1 = fun(inputT)(input =>
      join() o mapPar(
        asScalar() o mapSeq(
          reduceSeq(fun(x => fun(a => abs(float4)(x) + a) ), vectorize(4, 0.0f))
        ) o split(8192) o asVector(4)
      ) o split(32768) $ input
    )
    val phrase = TypeInference(intelDerivedNoWarp1, Map()).convertToPhrase
    val p = idealised.OpenMP.ProgramGenerator.makeCode(phrase)
    println(p.code)
    SyntaxChecker(p.code)
  }

}
