package apps

import rise.core.DSL._
import rise.core.TypeLevelDSL._
import rise.core.types._
import util.gen
import rise.core.HighLevelConstructs.reorderWithStride

//noinspection TypeAnnotation
class gemv extends shine.test_util.Tests {

  // we can use implicit type parameters and type annotations to specify the function type of mult
  val mult = implDT(dt => fun(x => x._1 * x._2) :: ((dt x dt) ->: dt))
  val add = fun(x => fun(y => x + y))
  val scal = implN(n =>
    fun(xs => fun(a =>
      mapSeq(fun(x => a * x), xs)
    )) :: (ArrayType(n, f32) ->: f32 ->: ArrayType(n, f32))
  )
  val dot = fun(xs => fun(ys =>
    zip(xs, ys) |> mapSeq(mult) |> reduceSeq(add, l(0.0f))
  ))

  val high_level = nFun((n, m) => fun(
    (m`.`n`.`f32) ->: (n`.`f32) ->: (m`.`f32) ->: f32 ->: f32 ->:
      (m`.`f32)
  )((mat, xs, ys, alpha, beta) =>
    zip(mapSeq(fun(row => alpha * dot(row, xs)), mat), scal(ys, beta)) |>
      mapSeq(fun(x => x._1 + x._2))
  ))

  object ocl {
    import rise.OpenCL.DSL._

    val fullMatrixVectorFusedOpenCL = nFun((n, m) => fun(
      (m`.`n`.`f32) ->: (n`.`f32) ->: (m`.`f32) ->: f32 ->: f32 ->:
        (m`.`f32)
    )((mat, xs, ys, alpha, beta) =>
      zip(mat, ys) |>
      mapWorkGroup(fun(t =>
        zip(xs, t._1) |>
        split(n) |>
        toLocalFun(mapLocal(
          reduceSeq(fun(a => fun(x => mult(x) + a)), l(0.0f))
        )) |>
        mapLocal(fun(x => (alpha * x) + (t._2 * beta)))
      )) |>
      join
    ))

    val fullMatrixVectorFusedOpenCLAMD = nFun((n, m) => fun(
      (m`.`n`.`f32) ->: (n`.`f32) ->: (m`.`f32) ->: f32 ->: f32 ->: (m`.`f32)
    )((mat, xs, ys, alpha, beta) =>
      zip(mat, ys) |>
      mapWorkGroup(fun(t =>
        zip(xs, t._1) |>
        reorderWithStride(128) |>
        split(n /^ 128) |>
        toLocalFun(mapLocal(
          reduceSeq(fun(a => fun(x => mult(x) + a)), l(0.0f))
        )) |>
        split(128) |>
        toLocalFun(mapLocal(reduceSeq(add, l(0.0f)))) |>
        mapLocal(fun(x => (alpha * x) + (t._2 * beta)))
      )) |>
      join
    ))

    val keplerBest = nFun((n, m) => fun(
      (m`.`n`.`f32) ->: (n`.`f32) ->: (m`.`f32) ->: f32 ->: f32 ->:
        (m`.`f32)
    )((mat, xs, ys, alpha, beta) =>
      zip(mat, ys) |>
      mapWorkGroup(fun(t =>
        zip(xs, t._1) |>
        reorderWithStride(128) |>
        split(n /^ 128) |>
        toLocalFun(mapLocal(
          reduceSeq(fun(a => fun(x => mult(x) + a)), l(0.0f))
        )) |>
        toLocalFun(reduceSeq(add, l(0.0f))) |>
        fun(x => (alpha * x) + (t._2 * beta))
      ))
    ))
  }

  object omp {
    import rise.OpenMP.DSL._

    val fullMatrixVectorFusedOpenMP = nFun((n, m) => fun(
      (m`.`n`.`f32) ->: (n`.`f32) ->: (m`.`f32) ->: f32 ->: f32 ->:
        (m`.`f32)
    )((mat, xs, ys, alpha, beta) =>
      zip(mat, ys) |>
      mapPar(fun(t =>
        zip(xs, t._1) |>
        split(n) |>
        mapSeq(reduceSeq(fun(a => fun(x => mult(x) + a)), l(0.0f))) |>
        mapSeq(fun(x => (alpha * x) + (t._2 * beta)))
      )) |>
      join
    ))
  }

  test("High level gemv type inference works") {
    val typed = infer(high_level)

    val N = typed.t.asInstanceOf[NatDepFunType[_ <: Type]].x
    val M = typed.t
      .asInstanceOf[NatDepFunType[_ <: Type]].t
      .asInstanceOf[NatDepFunType[_ <: Type]].x
    assertResult(
      DepFunType(N,
        DepFunType(M,
          ArrayType(M, ArrayType(N, f32)) ->:
            (ArrayType(N, f32) ->: (ArrayType(M, f32) ->:
            (f32 ->: (f32 ->: ArrayType(M, f32)))))
        ))) {
      typed.t
    }
  }

  test("High level gemv compiles to syntactically correct C") {
    gen.CProgram(high_level)
  }

  test("OpenCL gemv versions type inference works") {
    infer(ocl.fullMatrixVectorFusedOpenCL)
    infer(ocl.fullMatrixVectorFusedOpenCLAMD)
    infer(ocl.keplerBest)
  }

  test("OpenMP gemv versions type inference works") {
    infer(omp.fullMatrixVectorFusedOpenMP)
  }

  test("OpenMP gemv versions compiles to syntactically correct OpenMP") {
    gen.OpenMPProgram(omp.fullMatrixVectorFusedOpenMP)
  }

}
