package apps

import idealised.DPIA.Types.ExpType
import lift.core._
import lift.core.types._
import lift.core.DSL._
import lift.core.primitives._
import lift.core.HighLevelConstructs.reorderWithStride
import idealised.util.{SyntaxChecker, gen}

//noinspection TypeAnnotation
class asum extends idealised.util.Tests {

  def inputT(n : NatIdentifier) = ArrayType(n, float)
  val abs = tFun(t => foreignFun("my_abs", Seq("y"), "{ return fabs(y); }", t ->: t))
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
    gen.CProgram(high_level)
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

    gen.OpenMPProgram(intelDerivedNoWarp1)
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

    gen.OpenMPProgram(intelDerived2)
  }

  test("AMD/Nvidia second kernel derived compiles to syntactically correct OpenMP code") {
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

    gen.OpenMPProgram(amdNvidiaDerived2)
  }

  { // OpenCL code gen
    import lift.OpenCL.primitives._
    import idealised.OpenCL
    import idealised.OpenCL.PrivateMemory

    test("Intel derived no warp compiles to syntactically correct OpenCL code") {
      val intelDerivedNoWarp1 = nFun(n => fun(inputT(n))(input =>
        input |>
          split(32768) |>
          mapWorkGroup(
            asVector(4) >>
              split(8192) >>
              mapLocal(
                oclReduceSeq(PrivateMemory)(fun(x => fun(a => abs(float4)(x) + a)))(vectorFromScalar(l(0.0f)))
              ) >> asScalar
          ) |> join
      ))
      val phrase = idealised.DPIA.fromLift(infer(intelDerivedNoWarp1))
      val N = phrase.t.asInstanceOf[idealised.DPIA.`(nat)->`[ExpType]].x
      val p = OpenCL.KernelGenerator.makeCode(localSize = 128, globalSize = N)(phrase)
      println(p.code)
      SyntaxChecker.checkOpenCL(p.code)
    }

    test("Second kernel of Intel derived compiles to syntactically correct OpenCL code") {
      val intelDerived2 = nFun(n => fun(inputT(n))(input =>
        input |>
          split(2048) |>
          mapWorkGroup(
            split(2048) >>
              mapLocal(oclReduceSeq(PrivateMemory)(add)(l(0.0f)))
          ) |> join
      ))

      val phrase = idealised.DPIA.fromLift(infer(intelDerived2))
      val N = phrase.t.asInstanceOf[idealised.DPIA.`(nat)->`[ExpType]].x
      val p = OpenCL.KernelGenerator.makeCode(localSize = 128, globalSize = N)(phrase)
      println(p.code)
      SyntaxChecker.checkOpenCL(p.code)
    }

    test("Nvidia kernel derived compiles to syntactically correct OpenCL code") {
      val nvidiaDerived1 = nFun(n => fun(inputT(n))(input =>
        input |>
          split(2048 * 128) |>
          mapWorkGroup(
            reorderWithStride(128) >>
              split(2048) >>
              mapLocal(
                oclReduceSeq(PrivateMemory)(fun(x => fun(a => abs(float)(x) + a)))(l(0.0f))
              )
          ) |> join
      ))

      val phrase = idealised.DPIA.fromLift(infer(nvidiaDerived1))
      val N = phrase.t.asInstanceOf[idealised.DPIA.`(nat)->`[ExpType]].x
      val p = OpenCL.KernelGenerator.makeCode(localSize = 128, globalSize = N)(phrase)
      println(p.code)
      SyntaxChecker.checkOpenCL(p.code)
    }

    // FIXME
    ignore("AMD/Nvidia second kernel derived compiles to syntactically correct OpenCL code") {
      val amdNvidiaDerived2 = nFun(n => fun(inputT(n))(input =>
        input |>
          split(8192) |>
          mapWorkGroup(
            split(128) >>
              toLocal(reduceSeq(add)(l(0.0f))) >>
              iterate(6)(nFun(_ =>
                split(2) >>
                  toLocal(mapLocal(reduceSeq(add)(l(0.0f))))
              ))
          ) |> join
      ))

      val phrase = idealised.DPIA.fromLift(infer(amdNvidiaDerived2))
      val N = phrase.t.asInstanceOf[idealised.DPIA.`(nat)->`[ExpType]].x
      val p = OpenCL.KernelGenerator.makeCode(localSize = 128, globalSize = N)(phrase)
      println(p.code)
      SyntaxChecker.checkOpenCL(p.code)
    }

    test("AMD kernel derived compiles to syntactically correct OpenCL code") {
      val amdDerived1 = nFun(n => fun(inputT(n))(input =>
        input |>
          split(4096 * 128) |>
          mapWorkGroup(
            asVector(2) >>
              reorderWithStride(64) >>
              split(2048) >>
              mapLocal(
                oclReduceSeq(PrivateMemory)(fun(x => fun(a => abs(float2)(x) + a)))(vectorFromScalar(l(0.0f)))
              ) >> asScalar
          ) |> join
      ))

      val phrase = idealised.DPIA.fromLift(infer(amdDerived1))
      val N = phrase.t.asInstanceOf[idealised.DPIA.`(nat)->`[ExpType]].x
      val p = OpenCL.KernelGenerator.makeCode(localSize = 128, globalSize = N)(phrase)
      println(p.code)
      SyntaxChecker.checkOpenCL(p.code)
    }
  }
}
