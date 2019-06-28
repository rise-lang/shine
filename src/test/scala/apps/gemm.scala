package apps

import idealised.util.gen
import lift.core.DSL._
import lift.core.Nat
import lift.core.primitives._
import lift.core.types._

//noinspection TypeAnnotation
class gemm extends idealised.util.Tests {

  val epsilon = 1.0f

  def print2DArray(m: Array[Array[Float]]): Unit = {
    m.foreach( r => {
      println(r.map(x => f"$x%4.0f").reduce(_ + " " + _))
    } )
  }

  def print1DArray(m: Array[Float]): Unit = {
    println(m.map(x => f"$x%4.0f").reduce(_ + " " + _))
  }

  def matrixMatrixMultiply(A: Array[Array[Float]],
                           B: Array[Array[Float]],
                           C: Array[Array[Float]],
                           alpha: Float,
                           beta: Float) :  Array[Array[Float]] = {
    val aCols = A(0).length
    val aRows = A.length
    val bCols = B(0).length
    val res =  Array.ofDim[Float](aRows, bCols)

    if (A.head.length != B.length)
      throw new IllegalArgumentException

    @inline def computeRow(row: Int): Unit = {
      // while statements are much faster than for statements
      var col = 0
      while(col < bCols) { var i = 0; var sum = 0.0f
        while(i < aCols) {
          sum += A(row)(i) * B(i)(col)
          i += 1
        }

        res(row)(col) =  alpha * sum + C(row)(col) * beta
        col += 1
      }
    }

    (0 until aRows).par.foreach( computeRow )

    res
  }

  // we can use implicit type parameters and type annotations to specify the function type of mult
  val mult  = implT(dt => fun(x => x._1 * x._2) :: ((dt x dt) -> dt))
  val add   = fun(x => fun(y => x + y))
  val scal  = implN(n => fun(xs => fun(a => mapSeq(fun(x => a * x), xs))) :: (ArrayType(n, float) ->: float ->: ArrayType(n, float)))
  val dot = fun(x => foreignFun("dot", float4 ->: float4 ->: float)(x._1, x._2))

  val sequential =
    nFun((n, m, k) =>
      fun((n`.`k`.`float) ->: (k`.`m`.`float) ->: (n`.`m`.`float) ->: float ->: float ->: (n`.`m`.`float))
         ((a, b, c, alpha, beta) =>

         zip(a, c) |> mapSeq(fun(ac =>
           zip(transpose(b), ac._2) |> mapSeq(fun(bc =>
             zip(ac._1, bc._1) |>
               reduceSeq(fun( (y, acc) => acc + (y._1 * y._2)), l(0.0f)) |>
               fun(x => (x * alpha) + (beta * bc._2))
           ))
         ))
      )
    )

  object ocl {
    import lift.OpenCL.primitives._

    val p1: Nat = 2
    val p2: Nat = 2
    val p3: Nat = 4
    val vw: Nat = 4

    val zeros = implN(n => implN(m =>
      generate(fun(IndexType(m))(_ => generate(fun(IndexType(n))(_ => l(0.0f) )))) ))

    val mali_GEMM =
      nFun((n, m, k) =>
        fun((m`.`k`.`float) ->: (n`.`k`.`float) ->: (m`.`n`.`float) ->: float ->: float ->: (m`.`n`.`float))
        ((a, b, c, alpha, beta) =>

          zip( split(p2)(a), split(p2)(c) ) |>
            mapGlobal(0)(fun(ac =>
              zip( split(p2)(b), split(p1)(transpose(ac._2)) ) |>
                mapGlobal(1)(fun(bc =>
                  zip( split(p3)(transpose(ac._1)), split(p3)(transpose(bc._1)) ) |>
                    reduceSeq(fun((p236, p67) =>
                      zip(p67, transpose(p236._1)) |>
                        mapSeq(fun(p54 =>
                          zip(p54._1, transpose(p236._2)) |>
                            mapSeq(fun(p157 =>
                              zip(asVector(vw)(p54._2), asVector(vw)(p157._2)) |>
                                mapSeq(fun(x => p157._1 + dot(x)))
                            )) |> join
                        ))
                      ), zeros) |>
                    fun(p235 =>
                      zip(p235, transpose(bc._2)) |>
                        mapSeq(fun(p237 =>
                          zip(p237._1, p237._2) |>
                            mapSeq(fun(p64 => (p64._1 * alpha) + (p64._2 * beta) ))
                        ))
                    ) |> transpose
                )) |> join |> transpose
            )) |> join
        )
      )
  }

  test("High level gemm type inference works") {
    infer(sequential)
  }

  test("High level gemm compiles to syntactically correct C") {
    gen.CProgram(sequential)
  }

  test("OpenCL gemm versions type inference works") {
    infer(ocl.mali_GEMM)
  }

}
