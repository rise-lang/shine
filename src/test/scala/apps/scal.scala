package apps

import lift.core.DSL._
import lift.core.types._
import lift.core.primitives._
import idealised.util.gen

class scal extends idealised.util.Tests {

  private val simpleScal = nFun(n => fun(ArrayType(n, float))(input => fun(float)(alpha =>
    input |> mapSeq(fun(x => alpha * x))
  )))

  // TODO: fix equality of types (specifically, NatDependentFunctionType and TypeDependentFunctionType)
  ignore("Simple scal type inference works") {
    val typed = infer(simpleScal)

    assertResult(
      nFunT(n => FunctionType(ArrayType(n, float), FunctionType(float, ArrayType(n, float))))
    ) {
      typed.t
    }
  }

  // OpenMP
  test("scalIntel compiles to syntactically correct OpenMP") {
    import lift.OpenMP.primitives._

    val scalIntel = nFun(n => fun(ArrayType(n, float))(input => fun(float)(alpha =>
      input |>
        split(4 * 128 * 128) |>
        mapPar(
          asVector(4) >>
            split(128) >>
            mapSeq(mapSeq(
              fun(x => vectorFromScalar(alpha) * x)
            )) >> join >> asScalar
        ) |>
        join
    )))

    gen.OpenMPProgram(scalIntel)
  }

  test("scalIntel2 compiles to syntactically correct OpenMP") {
    import lift.OpenMP.primitives._

    val scalIntel2 = nFun(n => fun(ArrayType(n, float))(input => fun(float)(alpha =>
      input |>
        split(4 * 128 * 128) |>
        mapPar(
          asVector(4) >>
            mapSeq(
              fun(x => vectorFromScalar(alpha) * x)
            ) >> asScalar
        ) |> join
    )))

    gen.OpenMPProgram(scalIntel2)
  }
/*
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
  */
}
