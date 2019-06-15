package apps

import lift.core.DSL._
import lift.core.primitives._
import lift.core.types._
import idealised.util.gen
import lift.core.HighLevelConstructs.reorderWithStride

//noinspection TypeAnnotation
class gemv extends idealised.util.Tests {

  // we can use implicit type parameters and type annotations to specify the function type of mult
  val mult  = implT(dt => fun(x => x._1 * x._2) :: ((dt x dt) -> dt))
  val add   = fun(x => fun(y => x + y))
  val scal  = implN(n => fun(xs => fun(a => mapSeq(fun(x => a * x), xs))) :: (ArrayType(n, float) -> (float -> ArrayType(n, float))))
  val dot   = fun(xs => fun(ys => zip(xs, ys) |> mapSeq(mult) |> reduceSeq(add, l(0.0f))))

  val high_level =
    nFun((n, m) =>
      fun((m`.`n`.`float) ->: (n`.`float) ->: (m`.`float) ->: float ->: float ->: (m`.`float))
         ((mat, xs, ys, alpha, beta) =>

        zip(mapSeq(fun(row => alpha * dot(row, xs)), mat), scal(ys, beta)) |>
          mapSeq(fun(x => x._1 + x._2))

      ))

  test("High level gemv type inference works") {
    val typed = infer(high_level)

    val N = typed.t.asInstanceOf[NatDependentFunctionType[_ <: Type]].x
    val M = typed.t.asInstanceOf[NatDependentFunctionType[_ <: Type]].t.asInstanceOf[NatDependentFunctionType[_ <: Type]].x
    assertResult(
      NatDependentFunctionType(N,
        NatDependentFunctionType(M,
            ArrayType(M, ArrayType(N, float)) ->
              (ArrayType(N, float) -> (ArrayType(M, float) ->
                (float -> (float -> ArrayType(M, float)))))))) {
      typed.t
    }
    println(typed)
  }

  // C code gen
  test("High level gemv compiles to syntactically correct C") {
    gen.CProgram(high_level)
  }

  test("OpenCL gemv versions type inference works") {
    import lift.OpenCL.primitives._

    val fullMatrixVectorFusedOpenCL =
      nFun((n, m) =>
        fun((m `.` n `.` float) ->: (n `.` float) ->: (m `.` float) ->: float ->: float ->: (m `.` float))
           ((mat, xs, ys, alpha, beta) =>
          zip(mat, ys) |>
            mapWorkGroup(fun(t =>
              zip(xs, t._1) |>
                split(n) |>
                toLocal(mapLocal(reduceSeq(fun(x => fun(a => mult(x) * a)), l(0.0f)))) |>
                mapLocal(fun(x => (alpha * x) + (t._2 * beta)))
            )) |>
            join
        ))

    infer(fullMatrixVectorFusedOpenCL)

    val fullMatrixVectorFusedOpenCLAMD =
      nFun((n, m) =>
        fun((m `.` n `.` float) ->: (n `.` float) ->: (m `.` float) ->: float ->: float ->: (m `.` float))
           ((mat, xs, ys, alpha, beta) =>
          zip(mat, ys) |>
            mapWorkGroup(fun(t =>
              zip(xs, t._1) |>
                reorderWithStride(128) |>
                split(n /^ 128) |>
                toLocal(mapLocal(reduceSeq(fun(x => fun(a => mult(x) * a)), l(0.0f)))) |>
                split(128) |>
                toLocal(mapLocal(reduceSeq(add, l(0.0f)))) |>
                mapLocal(fun(x => (alpha * x) + (t._2 * beta)))
            )) |>
            join
           ))

    infer(fullMatrixVectorFusedOpenCLAMD)

    val keplerBest =
      nFun((n, m) =>
        fun((m `.` n `.` float) ->: (n `.` float) ->: (m `.` float) ->: float ->: float ->: (m `.` float))
           ((mat, xs, ys, alpha, beta) =>
          zip(mat, ys) |>
            mapWorkGroup(fun(t =>
              zip(xs, t._1) |>
                reorderWithStride(128) |>
                split(n /^ 128) |>
                toLocal(mapLocal(reduceSeq(fun(x => fun(a => mult(x) * a)), l(0.0f)))) |>
                toLocal(reduceSeq(add, l(0.0f))) |>
                fun(x => (alpha * x) + (t._2 * beta))
            ))
        ))

    infer(keplerBest)
  }

}
