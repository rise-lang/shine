package apps

import shine.DPIA.Types.{ExpType, read, write}
import shine.DPIA.Types.TypeCheck._
import rise.core.DSL._
import rise.core.types._
import rise.core.HighLevelConstructs.reorderWithStride
import util.gen

class dot extends test_util.Tests {

  private def xsT(N : NatIdentifier) = ArrayType(N, float)
  private def ysT(N : NatIdentifier) = ArrayType(N, float)

  private val mulT = fun(x => fst(x) * snd(x))
  private val add = fun(a => fun(x => a + x))

  private val simpleDotProduct = nFun(n => fun(xsT(n))(xs => fun(ysT(n))(ys =>
    zip(xs)(ys) |> toMemFun(mapSeq(mulT)) |> reduceSeq(add)(l(0.0f))
  )))

  test("Simple dot product type inference works") {
    val typed = infer(simpleDotProduct)

    val N = typed.t.asInstanceOf[NatDepFunType[_ <: Type]].x
    assertResult(DepFunType[NatKind, Type](N, FunType(xsT(N), FunType(ysT(N), float)))) {
      typed.t
    }
  }

  test("Simple dot product translation to phrase works and preserves types") {
    import shine.DPIA.Types.float
    import shine.DPIA._
    val phrase = shine.DPIA.fromRise(infer(simpleDotProduct))

    val N = phrase.t.asInstanceOf[`(nat)->:`[ExpType ->: ExpType]].x
    val dt = float
    assert(phrase.t `<=`
      (N `()->:` (exp"[$N.$dt, $read]" ->: exp"[$N.$dt, $read]" ->: exp"[$dt, $write]")))
  }

  // C
  test("Simple dot product compiles to syntactically correct C") {
    gen.CProgram(simpleDotProduct)
  }

  // OpenMP
  test("Dot product CPU vector 1 compiles to syntactically correct OpenMP") {
    import rise.OpenMP.DSL._

    val dotCPUVector1 = nFun(n => fun(xsT(n))(xs => fun(ysT(n))(ys =>
      zip(asVectorAligned(4)(xs))(asVectorAligned(4)(ys))
      |> split(2048 * 64)
      |> mapPar(
        split(2048) >>
        mapSeq(
          reduceSeq(fun(a => fun(x => a + mulT(x))))(vectorFromScalar(l(0.0f)))
        )
      ) |> join |> asScalar
    )))

    gen.OpenMPProgram(dotCPUVector1)
  }

  test("Intel derived no warp dot product 1 compiles to syntactically correct OpenMP") {
    import rise.OpenMP.DSL._

    val intelDerivedNoWarpDot1 = nFun(n =>
      fun(xsT(n))(xs => fun(ysT(n))(ys =>
        zip(xs |> asVectorAligned(4))(ys |> asVectorAligned(4))
        |> split(8192)
        |> mapPar(
          split(8192) >>
          mapSeq(
            reduceSeq(fun(a => fun(x => a + mulT(x))))(vectorFromScalar(l(0.0f)))
          )
        ) |> join |> asScalar
      )))

    gen.OpenMPProgram(intelDerivedNoWarpDot1)
  }

  test("Dot product CPU 1 compiles to syntactically correct OpenMP") {
    import rise.OpenMP.DSL._

    val dotCPU1 = nFun(n => fun(xsT(n))(xs => fun(ysT(n))(ys =>
      zip(xs)(ys) |>
        split(2048 * 128) |>
        mapPar(
          split(2048) >>
            mapSeq(
              reduceSeq(fun(a => fun(x => a + mulT(x))))(l(0.0f))
            )
        ) |> join
    )))

    gen.OpenMPProgram(dotCPU1)
  }

  test("Dot product CPU 2 compiles to syntactically correct OpenMP") {
    import rise.OpenMP.DSL._

    val dotCPU2 = nFun(n => fun(xsT(n))(in =>
      in |>
        split(128) |>
        mapPar(
          split(128) >>
            mapSeq(
              reduceSeq(add)(l(0.0f))
            )
        ) |> join
    ))

    gen.OpenMPProgram(dotCPU2)
  }

  { // OpenCL
    import rise.OpenCL.DSL._

    test("Intel derived no warp dot product 1 compiles to syntactically correct OpenCL") {
      val intelDerivedNoWarpDot1 = nFun(n => fun(xsT(n))(xs => fun(ysT(n))(ys =>
        zip(xs |> asVectorAligned(4))(ys |> asVectorAligned(4)) |>
          split(8192) |>
          mapWorkGroup(
            split(8192) >>
              mapLocal(
                oclReduceSeq(AddressSpace.Private)(fun(a => fun(x => a + mulT(x))))(vectorFromScalar(l(0.0f)))
              )
          ) |> join |> asScalar
      )))

      gen.OpenCLKernel(intelDerivedNoWarpDot1)
    }

    test("Dot product CPU 1 compiles to syntactically correct OpenCL") {
      val dotCPU1 = nFun(n => fun(xsT(n))(xs => fun(ysT(n))(ys =>
        zip(xs)(ys) |>
          split(2048 * 128) |>
          mapWorkGroup(
            split(2048) >>
              mapLocal(
                oclReduceSeq(AddressSpace.Private)(fun(a => fun(x => a + mulT(x))))(l(0.0f))
              )
          ) |> join
      )))

      gen.OpenCLKernel(dotCPU1)
    }

    test("Dot product CPU 2 compiles to syntactically correct OpenCL") {
      val dotCPU2 = nFun(n => fun(xsT(n))(in =>
        in |>
          split(128) |>
          mapWorkGroup(
            split(128) >>
              mapLocal(
                oclReduceSeq(AddressSpace.Private)(fun(a => fun(x => a + x)))(l(0.0f))
              )
          ) |> join
      ))

      gen.OpenCLKernel(dotCPU2)
    }

    test("Dot product 1 compiles to syntactically correct OpenCL") {
      val dotProduct1 = nFun(n => fun(xsT(n))(xs => fun(ysT(n))(ys =>
        zip(xs)(ys) |>
          split(2048 * 128) |>
          mapWorkGroup(
            reorderWithStride(128) >>
              split(2048) >>
              mapLocal(
                oclReduceSeq(AddressSpace.Private)(fun(a => fun(x => a + mulT(x))))(l(0.0f))
              )
          ) |> join
      )))

      gen.OpenCLKernel(dotProduct1)
    }

    // FIXME: SyntaxChecker fails
    ignore("Dot product 2 compiles to syntactically correct OpenCL") {
      val dotProduct2 = nFun(n => fun(xsT(n))(in =>
        in |>
          split(128) |>
          mapWorkGroup(
            split(2) >>
              toLocal(mapLocal(oclReduceSeq(AddressSpace.Private)(add)(l(0.0f)))) >>
              iterate(6)(nFun(_ =>
                split(2) >> toLocal(mapLocal(oclReduceSeq(AddressSpace.Private)(add)(l(0.0f))))
              ))
          ) |> join
      ))

      gen.OpenCLKernel(dotProduct2)
    }
  }
}
