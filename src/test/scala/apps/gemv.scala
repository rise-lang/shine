package apps

import rise.core.DSL._
import rise.core.primitives._
import Type._
import rise.core.types._
import util.gen
import HighLevelConstructs.reorderWithStride
import util.gen.c.function

//noinspection TypeAnnotation
class gemv extends test_util.Tests {

  // we can use implicit type parameters and type annotations to specify the function type of mult
  val mult = impl{ dt: DataType => fun(x => x._1 * x._2) :: ((dt x dt) ->: dt) }
  val add = fun(x => fun(y => x + y))
  val scal = impl { n: Nat =>
    fun(xs => fun(a =>
      mapSeq(fun(x => a * x))(xs)
    )) :: (ArrayType(n, f32) ->: f32 ->: ArrayType(n, f32))
  }
  val dot = fun(xs => fun(ys =>
    zip(xs)(ys) |> toMemFun(mapSeq(mult)) |> reduceSeq(add)(lf32(0.0f))
  ))

  val high_level = depFun((n: Nat, m: Nat) => fun(
    (m`.`n`.`f32) ->: (n`.`f32) ->: (m`.`f32) ->: f32 ->: f32 ->:
      (m`.`f32)
  )((mat, xs, ys, alpha, beta) =>
    toMem(zip(mapSeq(fun(row => alpha * dot(row, xs)))(mat))(scal(ys, beta))) |>
      mapSeq(fun(x => x._1 + x._2))
  ))

  object ocl {
    import rise.openCL.TypedDSL._

    val fullMatrixVectorFusedOpenCL = depFun((n: Nat, m: Nat) => fun(
      (m`.`n`.`f32) ->: (n`.`f32) ->: (m`.`f32) ->: f32 ->: f32 ->:
        (m`.`f32)
    )((mat, xs, ys, alpha, beta) =>
      zip(mat)(ys) |>
      mapWorkGroup(fun(t =>
        zip(xs)(t._1) |>
        split(n) |>
        toLocalFun(mapLocal(
          reduceSeq(fun(a => fun(x => mult(x) + a)))(lf32(0.0f))
        )) |>
        mapLocal(fun(x => (alpha * x) + (t._2 * beta)))
      )) |> join
    ))

    val fullMatrixVectorFusedOpenCLAMD = depFun((n: Nat, m: Nat) => fun(
      (m`.`n`.`f32) ->: (n`.`f32) ->: (m`.`f32) ->: f32 ->: f32 ->: (m`.`f32)
    )((mat, xs, ys, alpha, beta) =>
      zip(mat)(ys) |>
      mapWorkGroup(fun(t =>
        zip(xs)(t._1) |>
        reorderWithStride(128) |>
        split(n /^ 128) |>
        toLocalFun(mapLocal(
          reduceSeq(fun(a => fun(x => mult(x) + a)))(lf32(0.0f))
        )) |>
        split(128) |>
        toLocalFun(mapLocal(reduceSeq(add)(lf32(0.0f)))) |>
        mapLocal(fun(x => (alpha * x) + (t._2 * beta)))
      )) |> join
    ))

    val keplerBest = depFun((n: Nat, m: Nat) => fun(
      (m`.`n`.`f32) ->: (n`.`f32) ->: (m`.`f32) ->: f32 ->: f32 ->:
        (m`.`f32)
    )((mat, xs, ys, alpha, beta) =>
      zip(mat)(ys) |>
      mapWorkGroup(fun(t =>
      zip(xs)(t._1) |>
        reorderWithStride(128) |>
        split(n /^ 128) |>
        toLocalFun(mapLocal(
          reduceSeq(fun(a => fun(x => mult(x) + a)))(lf32(0.0f))
        )) |>
        toLocalFun(reduceSeq(add)(lf32(0.0f))) |>
        fun(x => (alpha * x) + (t._2 * beta))
      ))
    ))
  }

  object omp {
    import rise.openMP.primitives._

    val fullMatrixVectorFusedOpenMP = depFun((n: Nat, m: Nat) => fun(
      (m`.`n`.`f32) ->: (n`.`f32) ->: (m`.`f32) ->: f32 ->: f32 ->:
        (m`.`f32)
    )((mat, xs, ys, alpha, beta) =>
      zip(mat)(ys) |>
      mapPar(fun(t =>
      zip(xs)(t._1) |>
        split(n) |>
        toMemFun(mapSeq(reduceSeq(fun(a => fun(x => mult(x) + a)))(lf32(0.0f)))) |>
        mapSeq(fun(x => (alpha * x) + (t._2 * beta)))
      )) |> join
    ))
  }

  test("High level gemv type inference works") {
    val typed = high_level.toExpr

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
    function.asStringFromExpr(high_level)
  }

  test("OpenCL gemv versions type inference works") {
    ocl.fullMatrixVectorFusedOpenCL.toExpr
    ocl.fullMatrixVectorFusedOpenCLAMD.toExpr
    ocl.keplerBest.toExpr
  }

  test("OpenMP gemv versions type inference works") {
    omp.fullMatrixVectorFusedOpenMP.toExpr
  }

  test("OpenMP gemv versions compiles to syntactically correct OpenMP") {
    gen.openmp.function.asStringFromExpr(omp.fullMatrixVectorFusedOpenMP)
  }

}
