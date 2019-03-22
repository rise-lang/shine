package idealised.apps

import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker
import lift.arithmetic._

class scal extends idealised.util.Tests {

  private val simpleScal = nFun(n => fun(ArrayType(n, float))(input => fun(float)(alpha =>
    mapSeq(fun(x => alpha * x ), input)
  ) ))

  // TODO: fix equality of types (specifically, NatDependentFunctionType and TypeDependentFunctionType)
  ignore("Simple scal type inference works") {
    val typed = TypeInference(simpleScal, Map())

    assertResult(
      NatDependentFunctionType(n => FunctionType(ArrayType(n, float), FunctionType(float, ArrayType(n, float))))
    ) {
      typed.t.get
    }
  }

  // OpenMP
  test("scalIntel compiles to syntactically correct OpenMP") {
    import idealised.OpenMP.SurfaceLanguage.DSL._

    val scalIntel = nFun(n => fun(ArrayType(n, float))(input => fun(float)(alpha =>
      input :>>
        split(4 * 128 * 128) :>>
        mapPar(
          asVector(4) >>>
            split(128) >>>
            mapSeq(mapSeq(
              fun(x => vectorize(4, alpha) * x)
            )) >>> join >>> asScalar
        ) :>>
        join
    )))

    val phrase = idealised.DPIA.FromSurfaceLanguage(TypeInference(scalIntel, Map()))
    val program = idealised.OpenMP.ProgramGenerator.makeCode(phrase, "scalIntel")
    println(program.code)
  }

  test("scalIntel2 compiles to syntactically correct OpenMP") {
    import idealised.OpenMP.SurfaceLanguage.DSL._

    val scalIntel2 = nFun(n => fun(ArrayType(n, float))(input => fun(float)(alpha =>
      input :>>
        split(4 * 128 * 128) :>>
        mapPar(
          asVector(4) >>>
            mapSeq(
              fun(x => vectorize(4, alpha) * x)
            ) >>> asScalar
        ) :>> join
    )))

    val phrase = idealised.DPIA.FromSurfaceLanguage(TypeInference(scalIntel2, Map()))
    val program = idealised.OpenMP.ProgramGenerator.makeCode(phrase, "scalIntel2")
    println(program.code)
  }

  // OpenCL
  {
    import idealised.OpenCL.SurfaceLanguage.DSL._

    val scalWgLcl = (fst: ArithExpr, snd: ArithExpr) =>
      nFun(n => fun(ArrayType(n, float))(input => fun(float)(alpha =>
        input :>>
          split(fst) :>>
          mapWorkgroup(
            split(snd) >>>
              mapLocal(mapSeq(
                fun(x => alpha * x)
              )) >>> join
          ) :>> join
      )))

    test("vectorScal compiles to syntactically correct OpenCL") {
      val vectorScal = scalWgLcl(1024, 4)
      val phrase = idealised.DPIA.FromSurfaceLanguage(TypeInference(vectorScal, Map()))
      val p = idealised.OpenCL.KernelGenerator.makeCode(phrase)
      println(p.code)
      SyntaxChecker.checkOpenCL(p.code)
    }

    test("scalAMD compiles to syntactically correct OpenCL") {
      val scalAMD = scalWgLcl(128, 1)
      val phrase = idealised.DPIA.FromSurfaceLanguage(TypeInference(scalAMD, Map()))
      val p = idealised.OpenCL.KernelGenerator.makeCode(phrase)
      println(p.code)
      SyntaxChecker.checkOpenCL(p.code)
    }

    test("scalNvidia compiles to syntactically correct OpenCL") {
      val scalNvidia = scalWgLcl(2048, 1)
      val phrase = idealised.DPIA.FromSurfaceLanguage(TypeInference(scalNvidia, Map()))
      val p = idealised.OpenCL.KernelGenerator.makeCode(phrase)
      println(p.code)
      SyntaxChecker.checkOpenCL(p.code)
    }

    test("scalIntel compiles to syntactically correct OpenCL") {
      val scalIntel = nFun(n => fun(ArrayType(n, float))(input => fun(float)(alpha =>
        input :>>
          split(4 * 128 * 128) :>>
          mapWorkgroup(
            asVector(4) >>>
              split(128) >>>
              mapLocal(mapSeq(
                fun(x => vectorize(4, alpha) * x)
              )) >>> join >>> asScalar
          ) :>>
          join
      )))
      val phrase = idealised.DPIA.FromSurfaceLanguage(TypeInference(scalIntel, Map()))
      val p = idealised.OpenCL.KernelGenerator.makeCode(phrase)
      println(p.code)
      SyntaxChecker.checkOpenCL(p.code)
    }

  }
}
