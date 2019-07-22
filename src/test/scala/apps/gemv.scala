package apps

import lift.core.DSL._
import lift.core.primitives._
import lift.core.types._
import idealised.util.gen
import lift.core.HighLevelConstructs.reorderWithStride

//noinspection TypeAnnotation
class gemv extends idealised.util.Tests {

  // we can use implicit type parameters and type annotations to specify the function type of mult
  val mult  = implDT(dt => fun(x => x._1 * x._2) :: ((dt x dt)._R ->: dt._R))
  val add   = fun(x => fun(y => x + y))
  val scal  = implN(n => fun(xs => fun(a => mapSeq(fun(x => a * x), xs))) :: (ArrayType(n, float)._R ->: float._R ->: ArrayType(n, float)._R))
  val dot   = fun(xs => fun(ys => zip(xs, ys) |> mapSeq(mult) |> reduceSeq(add, l(0.0f))))

  val high_level =
    nFun((n, m) =>
      fun((m`.`n`.`float)._R ->: (n`.`float)._R ->: (m`.`float)._R ->: float._R ->: float._R ->: (m`.`float)._R)
         ((mat, xs, ys, alpha, beta) =>

        zip(mapSeq(fun(row => alpha * dot(row, xs)), mat), scal(ys, beta)) |>
          mapSeq(fun(x => x._1 + x._2))

      ))

  object ocl {
    import lift.OpenCL.primitives._

    val fullMatrixVectorFusedOpenCL =
      nFun((n, m) =>
        fun((m `.` n `.` float)._R ->: (n `.` float)._R ->: (m `.` float)._R ->: float._R ->: float._R ->: (m `.` float)._R)
        ((mat, xs, ys, alpha, beta) =>
          zip(mat, ys) |>
            mapWorkGroup(fun(t =>
              zip(xs, t._1) |>
                split(n) |>
                toLocal(mapLocal(reduceSeq(fun(x => fun(a => mult(x) + a)), l(0.0f)))) |>
                mapLocal(fun(x => (alpha * x) + (t._2 * beta)))
            )) |>
            join
        ))

    val fullMatrixVectorFusedOpenCLAMD =
      nFun((n, m) =>
        fun((m `.` n `.` float)._R ->: (n `.` float)._R ->: (m `.` float)._R ->: float._R ->: float._R ->: (m `.` float)._R)
        ((mat, xs, ys, alpha, beta) =>
          zip(mat, ys) |>
            mapWorkGroup(fun(t =>
              zip(xs, t._1) |>
                reorderWithStride(128) |>
                split(n /^ 128) |>
                toLocal(mapLocal(reduceSeq(fun(x => fun(a => mult(x) + a)), l(0.0f)))) |>
                split(128) |>
                toLocal(mapLocal(reduceSeq(add, l(0.0f)))) |>
                mapLocal(fun(x => (alpha * x) + (t._2 * beta)))
            )) |>
            join
        ))

    val keplerBest =
      nFun((n, m) =>
        fun((m `.` n `.` float)._R ->: (n `.` float)._R ->: (m `.` float)._R ->: float._R ->: float._R ->: (m `.` float)._R)
        ((mat, xs, ys, alpha, beta) =>
          zip(mat, ys) |>
            mapWorkGroup(fun(t =>
              zip(xs, t._1) |>
                reorderWithStride(128) |>
                split(n /^ 128) |>
                toLocal(mapLocal(reduceSeq(fun(x => fun(a => mult(x) + a)), l(0.0f)))) |>
                toLocal(reduceSeq(add, l(0.0f))) |>
                fun(x => (alpha * x) + (t._2 * beta))
            ))
        ))
  }

  object omp {
    import lift.OpenMP.primitives._

    val fullMatrixVectorFusedOpenMP =
      nFun((n, m) =>
        fun((m `.` n `.` float)._R ->: (n `.` float)._R ->: (m `.` float)._R ->: float._R ->: float._R ->: (m `.` float)._R)
        ((mat, xs, ys, alpha, beta) =>
          zip(mat, ys) |>
            mapPar(fun(t =>
              zip(xs, t._1) |>
                split(n) |>
                mapSeq(reduceSeq(fun(x => fun(a => mult(x) + a)), l(0.0f))) |>
                mapSeq(fun(x => (alpha * x) + (t._2 * beta)))
            )) |>
            join
        ))
  }

  test("High level gemv type inference works") {
    val typed = infer(high_level)

    val N = typed.t.asInstanceOf[NatDependentFunctionType[_ <: Type]].x
    val M = typed.t.asInstanceOf[NatDependentFunctionType[_ <: Type]].t.asInstanceOf[NatDependentFunctionType[_ <: Type]].x
    assertResult(
      NatDependentFunctionType(N,
        NatDependentFunctionType(M,
            ArrayType(M, ArrayType(N, float))._R ->:
              ArrayType(N, float)._R ->: (ArrayType(M, float)._R ->:
                float._R ->: float._R ->: ArrayType(M, float)._R)))) {
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
