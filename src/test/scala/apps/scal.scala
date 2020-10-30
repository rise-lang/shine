package apps

import rise.core.TypedDSL._
import rise.core.primitives._
import rise.core.TypeLevelDSL._
import rise.core.types._
import util.gen

class scal extends test_util.Tests {

  private val simpleScal = depFun((n: Nat) => fun(n`.`f32)(input => fun(f32)(alpha =>
    input |> mapSeq(fun(x => alpha * x)))
  ))

  test("Simple scal type inference works") {
    assert(
      expl((n: Nat) => ArrayType(n, f32) ->: f32 ->: ArrayType(n, f32))
        ==
        simpleScal.t
    )
  }

  // OpenMP
  test("scalIntel compiles to syntactically correct OpenMP") {
    import rise.openMP.primitives._

    val scalIntel = depFun((n: Nat) => fun(n`.`f32)(input => fun(f32)(alpha =>
      input |>
      split(4 * 128 * 128) |>
      mapPar(
        asVectorAligned(4) >>
        split(128) >>
        mapSeq(mapSeq(fun(x =>
          vectorFromScalar(alpha) * x
        ))) >> join >> asScalar
      ) |>
      join
    )))

    gen.OpenMPProgram(scalIntel)
  }

  test("scalIntel2 compiles to syntactically correct OpenMP") {
    import rise.openMP.primitives._

    val scalIntel2 = depFun((n: Nat) => fun(n`.`f32)(input => fun(f32)(alpha =>
      input |>
      split(4 * 128 * 128) |>
      mapPar(
        asVectorAligned(4) >>
        mapSeq(fun(x => vectorFromScalar(alpha) * x)) >>
        asScalar
      ) |> join
    )))

    gen.OpenMPProgram(scalIntel2)
  }

  // OpenCL
  {
    import rise.openCL.TypedDSL._

    val scalWgLcl = (fst: Nat, snd: Nat) =>
      depFun((n: Nat) => fun(ArrayType(n, f32))(input => fun(f32)(alpha =>
        input |>
        split(fst) |>
        mapWorkGroup(
          split(snd) >>
          mapLocal(mapSeq(fun(x => alpha * x))) >>
          join
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
      val scalIntel = depFun((n: Nat) => fun(n`.`f32)(input => fun(f32)(alpha =>
        input |>
        split(4 * 128 * 128) |>
        mapWorkGroup(
          asVectorAligned(4) >>
          split(128) >>
          mapLocal(mapSeq(fun(x => vectorFromScalar(alpha) * x))) >>
          join >> asScalar
        ) |>
        join
      )))

      gen.OpenCLKernel(scalIntel)
    }
  }
}
