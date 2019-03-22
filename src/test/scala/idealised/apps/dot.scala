package idealised.apps

import idealised.DPIA.Types.ExpType
import idealised.OpenCL.PrivateMemory
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.NatIdentifier
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker

class dot extends idealised.util.Tests {

  private def xsT(N : NatIdentifier) = ArrayType(N, float)
  private def ysT(N : NatIdentifier) = ArrayType(N, float)

  private val mult = fun(x => x._1 * x._2)
  private val add = fun((x, a) => x + a)

  private val simpleDotProduct = nFun(n => fun(xsT(n))(xs => fun(ysT(n))(ys =>
    zip(xs, ys) :>> mapSeq(mult) :>> reduceSeq(add, 0.0f)
  )))

  test("Simple dot product type inference works") {
    val typed = TypeInference(simpleDotProduct, Map())

    val N = typed.t.get.asInstanceOf[NatDependentFunctionType[_ <: Type]].n
    assertResult(NatDependentFunctionType(N, FunctionType(xsT(N), FunctionType(ysT(N), float)))) {
      typed.t.get
    }
  }

  test("Simple dot product translation to phrase works and preserves types") {
    import idealised.DPIA.Types.float
    import idealised.DPIA._
    val phrase = idealised.DPIA.FromSurfaceLanguage(TypeInference(simpleDotProduct, Map()))

    val N = phrase.t.asInstanceOf[`(nat)->`[ExpType -> ExpType]].n
    val dt = float
    assertResult(N -> (exp"[$N.$dt]" -> (exp"[$N.$dt]" -> exp"[$dt]"))) {
      phrase.t
    }
  }

  // C
  test("Simple dot product compiles to syntactically correct C") {
    val p = idealised.C.ProgramGenerator.makeCode(idealised.DPIA.FromSurfaceLanguage(TypeInference(simpleDotProduct, Map())))
    println(p.code)
    SyntaxChecker(p.code)
  }

  // OpenMP
  test("Dot product CPU vector 1 compiles to syntactically correct OpenMP") {
    import idealised.OpenMP.SurfaceLanguage.DSL._

    val dotCPUVector1 = nFun(n => fun(xsT(n))(xs => fun(ysT(n))(ys =>
      zip(
        xs :>> asVector(4),
        ys:>> asVector(4)
      ) :>>
        split(2048 * 64) :>>
        mapPar(
          split(2048) >>>
            mapSeq(
              reduceSeq(fun(x => fun(a => mult(x) + a)), vectorize(4, 0.0f))
            )
        ) :>> join :>> asScalar
    )))

    val phrase = idealised.DPIA.FromSurfaceLanguage(TypeInference(dotCPUVector1, Map()))
    val p = idealised.OpenMP.ProgramGenerator.makeCode(phrase)
    println(p.code)
    SyntaxChecker(p.code)
  }

  test("Intel derived no warp dot product 1 compiles to syntactically correct OpenMP") {
    import idealised.OpenMP.SurfaceLanguage.DSL._

    val intelDerivedNoWarpDot1 = nFun(n =>
      fun(xsT(n))(xs => fun(ysT(n))(ys =>
        zip(
          xs :>> asVector(4),
          ys:>> asVector(4)
        ) :>>
          split(8192) :>>
          mapPar(
            split(8192) >>>
              mapSeq(
                reduceSeq(fun(x => fun(a => mult(x) + a)), vectorize(4, 0.0f))
              )
          ) :>> join :>> asScalar
      )))

    val phrase = idealised.DPIA.FromSurfaceLanguage(TypeInference(intelDerivedNoWarpDot1, Map()))
    val p = idealised.OpenMP.ProgramGenerator.makeCode(phrase)
    println(p.code)
    SyntaxChecker(p.code)
  }

  test("Dot product CPU 1 compiles to syntactically correct OpenMP") {
    import idealised.OpenMP.SurfaceLanguage.DSL._

    val dotCPU1 = nFun(n => fun(xsT(n))(xs => fun(ysT(n))(ys =>
      zip(xs, ys) :>>
        split(2048 * 128) :>>
        mapPar(
          split(2048) >>>
            mapSeq(
              reduceSeq(fun(x => fun(a => mult(x) + a)), 0.0f)
            )
        ) :>> join
    )))

    val phrase = idealised.DPIA.FromSurfaceLanguage(TypeInference(dotCPU1, Map()))
    val p = idealised.OpenMP.ProgramGenerator.makeCode(phrase)
    println(p.code)
    SyntaxChecker(p.code)
  }

  test("Dot product CPU 2 compiles to syntactically correct OpenMP") {
    import idealised.OpenMP.SurfaceLanguage.DSL._

    val dotCPU2 = nFun(n => fun(xsT(n))(in =>
      in :>>
        split(128) :>>
        mapPar(
          split(128) >>>
            mapSeq(
              reduceSeq(fun(x => fun(a => x + a)), 0.0f)
            )
        ) :>> join
    ))

    val phrase = idealised.DPIA.FromSurfaceLanguage(TypeInference(dotCPU2, Map()))
    val p = idealised.OpenMP.ProgramGenerator.makeCode(phrase)
    println(p.code)
    SyntaxChecker(p.code)
  }

  test("Intel derived no warp dot product 1 compiles to syntactically correct OpenCL") {
    import idealised.OpenCL.SurfaceLanguage.DSL._

    val intelDerivedNoWarpDot1 = nFun(n => fun(xsT(n))(xs => fun(ysT(n))(ys =>
      zip(
        xs :>> asVector(4),
        ys:>> asVector(4)
      ) :>>
        split(8192) :>>
        mapWorkgroup(
          split(8192) >>>
            mapLocal(
              oclReduceSeq(fun(x => fun(a => mult(x) + a)), vectorize(4, 0.0f), PrivateMemory)
            )
        ) :>> join :>> asScalar
    )))

    val phrase = idealised.DPIA.FromSurfaceLanguage(TypeInference(intelDerivedNoWarpDot1, Map()))
    val p = idealised.OpenCL.KernelGenerator.makeCode(phrase)
    println(p.code)
    SyntaxChecker.checkOpenCL(p.code)
  }

  test("Dot product CPU 1 compiles to syntactically correct OpenCL") {
    import idealised.OpenCL.SurfaceLanguage.DSL._

    val dotCPU1 = nFun(n => fun(xsT(n))(xs => fun(ysT(n))(ys =>
      zip(xs, ys) :>>
        split(2048 * 128) :>>
        mapWorkgroup(
          split(2048) >>>
            mapLocal(
              oclReduceSeq(fun(x => fun(a => mult(x) + a)), 0.0f, PrivateMemory)
            )
        ) :>> join
    )))

    val phrase = idealised.DPIA.FromSurfaceLanguage(TypeInference(dotCPU1, Map()))
    val p = idealised.OpenCL.KernelGenerator.makeCode(phrase)
    println(p.code)
    SyntaxChecker.checkOpenCL(p.code)
  }

  test("Dot product CPU 2 compiles to syntactically correct OpenCL") {
    import idealised.OpenCL.SurfaceLanguage.DSL._

    val dotCPU2 = nFun(n => fun(xsT(n))(in =>
      in :>>
        split(128) :>>
        mapWorkgroup(
          split(128) >>>
            mapLocal(
              oclReduceSeq(fun(x => fun(a => x + a)), 0.0f, PrivateMemory)
            )
        ) :>> join
    ))

    val phrase = idealised.DPIA.FromSurfaceLanguage(TypeInference(dotCPU2, Map()))
    val p = idealised.OpenCL.KernelGenerator.makeCode(phrase)
    println(p.code)
    SyntaxChecker.checkOpenCL(p.code)
  }

  test("Dot product 1 compiles to syntactically correct OpenCL") {
    import idealised.OpenCL.SurfaceLanguage.DSL._

    val dotProduct1 = nFun(n => fun(xsT(n))(xs => fun(ysT(n))(ys =>
      zip(xs, ys) :>>
        split(2048 * 128) :>>
        mapWorkgroup(
          reorderWithStride(128) >>>
            split(2048) >>>
            mapLocal(
              oclReduceSeq(fun(x => fun(a => mult(x) + a)), 0.0f, PrivateMemory)
            )
        ) :>> join
    )))

    val phrase = idealised.DPIA.FromSurfaceLanguage(TypeInference(dotProduct1, Map()))
    val p = idealised.OpenCL.KernelGenerator.makeCode(phrase)
    println(p.code)
    SyntaxChecker.checkOpenCL(p.code)
  }

  ignore ("Dot product 2 compiles to syntactically correct OpenCL") {
    import idealised.OpenCL.SurfaceLanguage.DSL._

    val dotProduct2 = nFun(n => fun(xsT(n))(in =>
      in :>>
        split(128) :>>
        mapWorkgroup(
          split(2) >>>
          toLocal(mapLocal(oclReduceSeq(add, 0.0f, PrivateMemory))) >>>
          iterate(6,
            split(2) >>> toLocal(mapLocal(oclReduceSeq(add, 0.0f, PrivateMemory)))
          )
        ) :>> join
    ))

    val phrase = idealised.DPIA.FromSurfaceLanguage(TypeInference(dotProduct2, Map()))
    val p = idealised.OpenCL.KernelGenerator.makeCode(phrase)
    println(p.code)
    SyntaxChecker.checkOpenCL(p.code)
  }

}
