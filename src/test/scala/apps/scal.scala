package apps

import rise.core.DSL._
import rise.core.primitives._
import Type._
import rise.core.types._
import util.gen
import rise.core.types.DataType._

class scal extends test_util.Tests {

  private val simpleScal = depFun((n: Nat) => fun(n`.`f32)(input => fun(f32)(alpha =>
    input |> mapSeq(fun(x => alpha * x)))
  ))

  test("Simple scal type inference works") {
    assert(
      expl((n: Nat) => ArrayType(n, f32) ->: f32 ->: ArrayType(n, f32))
        =~=
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

    gen.openmp.function.asStringFromExpr(scalIntel)
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

    gen.openmp.function.asStringFromExpr(scalIntel2)
  }

  // OpenCL
  {
    import rise.openCL.DSL._

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
      gen.opencl.kernel.fromExpr(vectorScal)
    }

    test("scalAMD compiles to syntactically correct OpenCL") {
      val scalAMD = scalWgLcl(128, 1)
      gen.opencl.kernel.fromExpr(scalAMD)
    }

    test("scalNvidia compiles to syntactically correct OpenCL") {
      val scalNvidia = scalWgLcl(2048, 1)
      gen.opencl.kernel.fromExpr(scalNvidia)
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

      gen.opencl.kernel.fromExpr(scalIntel)
    }
  }
}
