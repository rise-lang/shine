package apps

import lift.core._
import lift.core.types._
import lift.core.DSL._
import lift.core.primitives._
import idealised.util.SyntaxChecker
import idealised.{C, OpenMP}

class asum extends idealised.util.Tests {

  def inputT(n : NatIdentifier) = ArrayType(n, float)
  val abs = tFun(t => foreignFun("my_abs", Seq("y"), "{ return fabs(y); }", t -> t))
  val fabs = abs(float)
  val add = fun(x => fun(a => x + a))

  val high_level = nFun(n => fun(inputT(n))(input =>
    input |> map(fabs) |> reduceSeq(add)(l(0.0f))
  ))

  test("High level asum type inference works") {
    val typed = infer(high_level)

    val N = typed.t.asInstanceOf[NatDependentFunctionType[_ <: Type]].x
    assertResult(NatDependentFunctionType(N, FunctionType(inputT(N), float))) {
      typed.t
    }
  }

  // C code gen
  test("High level asum compiles to syntactically correct C") {
    val p = C.ProgramGenerator.makeCode(idealised.DPIA.fromLift(infer(high_level)))
    println(p.code)
    SyntaxChecker(p.code)
  }

  // OpenMP code gen
  test("Intel derived no warp compiles to syntactically correct OpenMP code") {
    import lift.OpenMP.primitives._

    val intelDerivedNoWarp1 = nFun(n => fun(inputT(n))(input =>
      input |>
        split(32768) |>
        mapPar(
          asVector(4) >>
          split(8192) >>
          mapSeq(
            reduceSeq(fun(x => fun(a => abs(float4)(x) + a)))(vectorFromScalar(l(0.0f)))
          ) >> asScalar
        ) |> join
    ))
    val typed_e = infer(intelDerivedNoWarp1)
    val phrase = idealised.DPIA.fromLift(typed_e)
    val p = OpenMP.ProgramGenerator.makeCode(phrase)
    println(p.code)
    SyntaxChecker(p.code)
  }

  test("Second kernel of Intel derived compiles to syntactically correct OpenMP code") {
    import lift.OpenMP.primitives._

    val intelDerived2 = nFun(n => fun(inputT(n))(input =>
      input |>
        split(2048) |>
        mapPar(
          split(2048) >> mapSeq(reduceSeq(add)(l(0.0f)))
        ) |> join
    ))
    val phrase = idealised.DPIA.fromLift(infer(intelDerived2))
    val p = OpenMP.ProgramGenerator.makeCode(phrase)
    println(p.code)
    SyntaxChecker(p.code)
  }

  // REASON: iterate was changed, incompatible with SurfaceLanguage
  ignore("AMD/Nvidia second kernel derived compiles to syntactically correct OpenMP code") {
    import lift.OpenMP.primitives._

    val amdNvidiaDerived2 = nFun(n => fun(inputT(n))(input =>
      input |>
        split(8192) |>
        mapPar(
          split(128) >>
            mapSeq(reduceSeq(add)(l(0.0f))) >>
            iterate(6)(nFun(_ =>
              split(2) >>
                mapSeq(reduceSeq(add)(l(0.0f))))
            )
        ) |> join
    ))
    val phrase = idealised.DPIA.fromLift(infer(amdNvidiaDerived2))
    val p = OpenMP.ProgramGenerator.makeCode(phrase)
    println(p.code)
    SyntaxChecker(p.code)
  }
/*
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
    val phrase = idealised.DPIA.FromSurfaceLanguage(typed)
    val N = typed.t.get.asInstanceOf[NatDependentFunctionType[DataType]].n
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
    val phrase = idealised.DPIA.FromSurfaceLanguage(TypeInference(intelDerived2, Map()))
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
          reorderWithStride(Cst(128)) >>>
            split(2048) >>>
            mapLocal(
              oclReduceSeq(fun(x => fun(a => abs(float)(x) + a)), 0.0f, OpenCL.PrivateMemory)
            )
        ) :>> join
    ))
    val phrase = idealised.DPIA.FromSurfaceLanguage(TypeInference(nvidiaDerived1, Map()))
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
            iterate(6, nFun(_ =>
              split(2) >>>
                toLocal(mapLocal(reduceSeq(add, 0.0f))))
            )
        ) :>> join
    ))
    val phrase = idealised.DPIA.FromSurfaceLanguage(TypeInference(amdNvidiaDerived2, Map()))
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
          reorderWithStride(Cst(64)) >>>
            split(2048) >>>
            mapLocal(
              oclReduceSeq(fun(x => fun(a => abs(float2)(x) + a)), vectorize(2, 0.0f), OpenCL.PrivateMemory)
            ) >>> asScalar
        ) :>> join
    ))
    val phrase = idealised.DPIA.FromSurfaceLanguage(TypeInference(amdDerived1, Map()))
    val N = phrase.t.asInstanceOf[`(nat)->`[ExpType]].n
    val p = OpenCL.KernelGenerator.makeCode(localSize = 128, globalSize = N)(phrase)
    println(p.code)
    SyntaxChecker.checkOpenCL(p.code)
  }
  */
}
