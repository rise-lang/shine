package apps

import lift.core._
import lift.core.DSL._
import lift.core.types._
import lift.core.primitives._
import idealised.util.gen

class scal extends idealised.util.Tests {

  private val simpleScal = nFun(n => fun(ArrayType(n, float))(input => fun(float)(alpha =>
    input |> mapSeq(fun(x => alpha * x))
  )))

  test("Simple scal type inference works") {
    val typed = infer(simpleScal)

    assert(
      nFunT(n => ArrayType(n, float)._R ->: float._R ->: ArrayType(n, float)._R)
        ==
      typed.t
    )
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

  // OpenCL
  {
    import lift.OpenCL.primitives._

    val scalWgLcl = (fst: Nat, snd: Nat) =>
      nFun(n => fun(ArrayType(n, float))(input => fun(float)(alpha =>
        input |>
          split(fst) |>
          mapWorkGroup(
            split(snd) >>
              mapLocal(mapSeq(
                fun(x => alpha * x)
              )) >> join
          ) |> join
      )))

    test("vectorScal compiles to syntactically correct OpenCL") {
      val vectorScal = scalWgLcl(1024, 4)
      gen.OpenCLKernel(vectorScal)
    }

    test("scalAMD compiles to syntactically correct OpenCL") {
      val scalAMD = scalWgLcl(128, 1)
      gen.OpenCLKernel(scalAMD)
    }

    test("scalNvidia compiles to syntactically correct OpenCL") {
      val scalNvidia = scalWgLcl(2048, 1)
      gen.OpenCLKernel(scalNvidia)
    }

    test("scalIntel compiles to syntactically correct OpenCL") {
      val scalIntel = nFun(n => fun(ArrayType(n, float))(input => fun(float)(alpha =>
        input |>
          split(4 * 128 * 128) |>
          mapWorkGroup(
            asVector(4) >>
              split(128) >>
              mapLocal(mapSeq(
                fun(x => vectorFromScalar(alpha) * x)
              )) >> join >> asScalar
          ) |>
          join
      )))
      gen.OpenCLKernel(scalIntel)
    }
  }
}
