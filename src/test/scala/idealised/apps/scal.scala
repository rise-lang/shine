package idealised.apps

import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker
import lift.arithmetic._

class scal extends idealised.util.Tests {

  private val N = SizeVar("N")
  private val inputT = ArrayType(N, float)

  private val simpleScal = fun(inputT)(input => fun(float)(alpha =>
    mapSeq(fun(x => alpha * x ), input)
  ) )

  test("Simple scal type inference works") {
    val typed = TypeInference(simpleScal, Map())

    assertResult(FunctionType(inputT, FunctionType(float, inputT))) {
      typed.t.get
    }
  }

  // OpenMP
  test("scalIntel compiles to syntactically correct OpenMP") {
    import idealised.OpenMP.SurfaceLanguage.DSL._

    val scalIntel = fun(inputT)(input => fun(float)(alpha =>
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
    ))

    val phrase = TypeInference(scalIntel, Map()).toPhrase
    val program = idealised.OpenMP.ProgramGenerator.makeCode(phrase, "scalIntel")
    println(program.code)
  }

  test("scalIntel2 compiles to syntactically correct OpenMP") {
    import idealised.OpenMP.SurfaceLanguage.DSL._

    val scalIntel2 = fun(inputT)(input => fun(float)(alpha =>
      input :>>
        split(4 * 128 * 128) :>>
        mapPar(
          asVector(4) >>>
            mapSeq(
              fun(x => vectorize(4, alpha) * x)
            ) >>> asScalar
        ) :>> join
    ))

    val phrase = TypeInference(scalIntel2, Map()).toPhrase
    val program = idealised.OpenMP.ProgramGenerator.makeCode(phrase, "scalIntel2")
    println(program.code)
  }

  // OpenCL
  {
    import idealised.OpenCL.SurfaceLanguage.DSL._

    val scalWgLcl = (fst: ArithExpr, snd: ArithExpr) =>
      fun(inputT)(input => fun(float)(alpha =>
        input :>>
          split(fst) :>>
          mapWorkgroup(
            split(snd) >>>
              mapLocal(mapSeq(
                fun(x => alpha * x)
              )) >>> join
          ) :>> join
      ))

    test("vectorScal compiles to syntactically correct OpenCL") {
      val vectorScal = scalWgLcl(1024, 4)
      val phrase = TypeInference(vectorScal, Map()).toPhrase
      val p = idealised.OpenCL.KernelGenerator.makeCode(phrase, ?, ?)
      println(p.code)
      SyntaxChecker.checkOpenCL(p.code)
    }

    test("scalAMD compiles to syntactically correct OpenCL") {
      val scalAMD = scalWgLcl(128, 1)
      val phrase = TypeInference(scalAMD, Map()).toPhrase
      val p = idealised.OpenCL.KernelGenerator.makeCode(phrase, ?, ?)
      println(p.code)
      SyntaxChecker.checkOpenCL(p.code)
    }

    test("scalNvidia compiles to syntactically correct OpenCL") {
      val scalNvidia = scalWgLcl(2048, 1)
      val phrase = TypeInference(scalNvidia, Map()).toPhrase
      val p = idealised.OpenCL.KernelGenerator.makeCode(phrase, ?, ?)
      println(p.code)
      SyntaxChecker.checkOpenCL(p.code)
    }

    test("scalIntel compiles to syntactically correct OpenCL") {
      val scalIntel = fun(inputT)(input => fun(float)(alpha =>
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
      ))
      val phrase = TypeInference(scalIntel, Map()).toPhrase
      val p = idealised.OpenCL.KernelGenerator.makeCode(phrase, ?, ?)
      println(p.code)
      SyntaxChecker.checkOpenCL(p.code)
    }

  }
}
