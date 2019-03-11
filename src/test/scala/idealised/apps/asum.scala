package idealised.apps

import idealised.DPIA._
import idealised.DPIA.Types.ExpType
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.NatIdentifier
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker
import idealised.{C, OpenCL, OpenMP}

class asum extends idealised.util.Tests {

  def inputT(n : NatIdentifier) = ArrayType(n, float)
  val abs = (t: DataType) => fun(x => foreignFun(t, "my_abs", (t, "y"), "{ return fabs(y); }", x))
  val fabs = abs(float)
  val add = fun(x => fun(a => x + a))

  val high_level = nFun(n => fun(inputT(n))(input =>
    input :>> map(fabs) :>> reduceSeq(add, 0.0f) ))

  test("High level asum type inference works") {
    val typed = TypeInference(high_level, Map())

    val N = typed.t.get.n
    assertResult(NatDependentFunctionType(N, FunctionType(inputT(N), float))) {
      typed.t.get
    }
  }

  // C code gen
  test("High level asum compiles to syntactically correct C") {
    val p = C.ProgramGenerator.makeCode(TypeInference(high_level, Map()).toPhrase)
    println(p.code)
    SyntaxChecker(p.code)
  }

  // OpenMP code gen
  test("Intel derived no warp compiles to syntactically correct OpenMP code") {
    import OpenMP.SurfaceLanguage.DSL._

    val intelDerivedNoWarp1 = nFun(n => fun(inputT(n))(input =>
      input :>>
        split(32768) :>>
        mapPar(
          asVector(4) >>>
          split(8192) >>>
          mapSeq(
            reduceSeq(fun(x => fun(a => abs(float4)(x) + a) ), vectorize(4, 0.0f))
          ) >>> asScalar
        ) :>> join
    ))
    val phrase = TypeInference(intelDerivedNoWarp1, Map()).convertToPhrase
    val p = OpenMP.ProgramGenerator.makeCode(phrase)
    println(p.code)
    SyntaxChecker(p.code)
  }

  test("Second kernel of Intel derived compiles to syntactically correct OpenMP code") {
    import OpenMP.SurfaceLanguage.DSL._

    val intelDerived2 = nFun(n => fun(inputT(n))(input =>
      input :>>
        split(2048) :>>
        mapPar(
          split(2048) >>> mapSeq(reduceSeq(add, 0.0f))
        ) :>> join
    ))
    val phrase = TypeInference(intelDerived2, Map()).convertToPhrase
    val p = OpenMP.ProgramGenerator.makeCode(phrase)
    println(p.code)
    SyntaxChecker(p.code)
  }

  test("AMD/Nvidia second kernel derived compiles to syntactically correct OpenMP code") {
    import OpenMP.SurfaceLanguage.DSL._

    val amdNvidiaDerived2 = nFun(n => fun(inputT(n))(input =>
      input :>>
        split(8192) :>>
        mapPar(
          split(128) >>>
            mapSeq(reduceSeq(add, 0.0f)) >>>
            iterate(6,
              split(2) >>>
                mapSeq(reduceSeq(add, 0.0f))
            )
        ) :>> join
    ))
    val phrase = TypeInference(amdNvidiaDerived2, Map()).convertToPhrase
    val p = OpenMP.ProgramGenerator.makeCode(phrase)
    println(p.code)
    SyntaxChecker(p.code)
  }

  // OpenCL code gen
  test("Intel derived no warp compiles to syntactically correct OpenCL code") {
    import OpenCL.SurfaceLanguage.DSL._

    val intelDerivedNoWarp1 = nFun(n => fun(inputT(n))(input =>
      input :>>
        split(32768) :>>
        mapWorkgroup(
          asVector(4) >>>
            split(8192) >>>
            mapLocal(
              oclReduceSeq(fun(x => fun(a => abs(float4)(x) + a) ), vectorize(4, 0.0f), OpenCL.PrivateMemory)
            ) >>> asScalar
        ) :>> join
    ))
    val typed = TypeInference(intelDerivedNoWarp1, Map())
    val phrase = typed.convertToPhrase
    val N = typed.t.get.n
    assert(N == phrase.t.asInstanceOf[`(nat)->`[ExpType]].n)
    val p = OpenCL.KernelGenerator.makeCode(localSize = 128, globalSize = N)(phrase)
    println(p.code)
    SyntaxChecker.checkOpenCL(p.code)
  }

  test("Second kernel of Intel derived compiles to syntactically correct OpenCL code") {
    import OpenCL.SurfaceLanguage.DSL._

    val intelDerived2 = nFun(n => fun(inputT(n))(input =>
      input :>>
        split(2048) :>>
        mapWorkgroup(
          split(2048) >>>
            mapLocal(oclReduceSeq(add, 0.0f, OpenCL.PrivateMemory))
        ) :>> join
    ))
    val phrase = TypeInference(intelDerived2, Map()).convertToPhrase
    val N = phrase.t.asInstanceOf[`(nat)->`[ExpType]].n
    val p = OpenCL.KernelGenerator.makeCode(localSize = 128, globalSize = N)(phrase)
    println(p.code)
    SyntaxChecker.checkOpenCL(p.code)
  }

  test("Nvidia kernel derived compiles to syntactically correct OpenCL code") {
    import OpenCL.SurfaceLanguage.DSL._

    val nvidiaDerived1 = nFun(n => fun(inputT(n))(input =>
      input :>>
        split(2048 * 128) :>>
        mapWorkgroup(
          reorderWithStride(128) >>>
            split(2048) >>>
            mapLocal(
              oclReduceSeq(fun(x => fun(a => abs(float)(x) + a)), 0.0f, OpenCL.PrivateMemory)
            )
        ) :>> join
    ))
    val phrase = TypeInference(nvidiaDerived1, Map()).convertToPhrase
    val N = phrase.t.asInstanceOf[`(nat)->`[ExpType]].n
    val p = OpenCL.KernelGenerator.makeCode(localSize = 128, globalSize = N)(phrase)
    println(p.code)
    SyntaxChecker.checkOpenCL(p.code)
  }

  ignore("AMD/Nvidia second kernel derived compiles to syntactically correct OpenCL code") {
    import OpenCL.SurfaceLanguage.DSL._

    val amdNvidiaDerived2 = nFun(n => fun(inputT(n))(input =>
      input :>>
        split(8192) :>>
        mapWorkgroup(
          split(128) >>>
            toLocal(mapLocal(reduceSeq(add, 0.0f))) >>>
            iterate(6,
              split(2) >>>
                toLocal(mapLocal(reduceSeq(add, 0.0f)))
            )
        ) :>> join
    ))
    val phrase = TypeInference(amdNvidiaDerived2, Map()).convertToPhrase
    val N = phrase.t.asInstanceOf[`(nat)->`[ExpType]].n
    val p = OpenCL.KernelGenerator.makeCode(localSize = 128, globalSize = N)(phrase)
    println(p.code)
    SyntaxChecker.checkOpenCL(p.code)
  }

  test("AMD kernel derived compiles to syntactically correct OpenCL code") {
    import OpenCL.SurfaceLanguage.DSL._

    val amdDerived1 = nFun(n => fun(inputT(n))(input =>
      input :>>
        split(4096 * 128) :>>
        mapWorkgroup(
          asVector(2) >>>
          reorderWithStride(64) >>>
            split(2048) >>>
            mapLocal(
              oclReduceSeq(fun(x => fun(a => abs(float2)(x) + a)), vectorize(2, 0.0f), OpenCL.PrivateMemory)
            ) >>> asScalar
        ) :>> join
    ))
    val phrase = TypeInference(amdDerived1, Map()).convertToPhrase
    val N = phrase.t.asInstanceOf[`(nat)->`[ExpType]].n
    val p = OpenCL.KernelGenerator.makeCode(localSize = 128, globalSize = N)(phrase)
    println(p.code)
    SyntaxChecker.checkOpenCL(p.code)
  }
}
