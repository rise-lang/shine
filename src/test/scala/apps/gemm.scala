package apps

import util.gen
import idealised.OpenCL.{GlobalSize, LocalSize}
import lift.arithmetic.Cst
import lift.core.DSL._
import lift.core.{Expr, Nat}
import lift.core.HighLevelConstructs._
import lift.core.primitives._
import lift.core.types._

//noinspection TypeAnnotation
class gemm extends util.TestsWithExecutor {

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
  val mult  = implDT(dt => fun(x => x._1 * x._2) :: ((dt x dt) ->: dt))
  val add   = fun(x => fun(y => x + y))
  val scal  = implN(n => fun(xs => fun(a => mapSeq(fun(x => a * x), xs))) :: (ArrayType(n, float) ->: float ->: ArrayType(n, float)))
  val dot = fun(x => foreignFun("dot", float4 ->: float4 ->: float)(x._1, x._2))
  def id: Expr = fun(x => x)

  val sequential =
    nFun((n, m, k) =>
      fun((n`.`k`.`float) ->: (k`.`m`.`float) ->: (n`.`m`.`float) ->: float ->: float ->: (n`.`m`.`float))
         ((a, b, c, alpha, beta) =>

         zip(a, c) |> mapSeq(fun(ac =>
           zip(transpose(b), ac._2) |> mapSeq(fun(bc =>
             zip(ac._1, bc._1) |>
               reduceSeq(fun( (acc, y) => acc + (y._1 * y._2)), l(0.0f)) |>
               fun(x => (x * alpha) + (beta * bc._2))
           ))
         ))
      )
    )

  object ocl {
    import lift.OpenCL.primitives._

    val sequential =
      nFun((n, m, k) =>
        fun((m`.`k`.`float) ->: (k`.`n`.`float) ->: (m`.`n`.`float) ->: float ->: float ->: (m`.`n`.`float))
        ((a, b, c, alpha, beta) =>

          zip(a, c) |> mapSeq(fun(ac =>
            zip(transpose(b), ac._2) |> mapSeq(fun(bc =>
              zip(ac._1, bc._1) |>
                oclReduceSeq(AddressSpace.Private)(fun( (acc, y) => acc + (y._1 * y._2)), l(0.0f)) |>
                fun(x => (x * alpha) + (beta * bc._2))
            ))
          ))
        )
      )

    val mali_GEMM = {
      val p1: Nat = 2
      val p2: Nat = 2
      val p3: Nat = 4
      val vw: Nat = 4

      val write_zeros = implN(n => implN(m =>
        generate(fun(IndexType(m))(_ => generate(fun(IndexType(n))(_ => l(0.0f)))))
        |> mapSeq(mapSeq(id))))

      nFun((n, m, k) =>
        fun((m `.` k `.` float) ->: (n `.` k `.` float) ->: (m `.` n `.` float) ->: float ->: float ->: (m `.` n `.` float))
        ((A, B, C, alpha, beta) =>

          zip(split(p2)(A), split(p2)(C)) |>
            mapGlobal(0)(fun(ac =>
              zip(split(p2)(B), split(p1)(transpose(ac._2))) |>
                mapGlobal(1)(fun(bc =>
                  zip(split(p3)(transpose(ac._1)), split(p3)(transpose(bc._1))) |>
                    oclReduceSeq(AddressSpace.Private)(fun((p67, p236) =>
                      zip(p67, transpose(p236._1)) |>
                        mapSeq(fun(p54 =>
                          zip(p54._1, transpose(p236._2)) |>
                            mapSeq(fun(p157 =>
                              zip(asVector(vw)(p54._2), asVector(vw)(p157._2)) |>
                                mapSeq(fun(x => p157._1 + dot(x)))
                            )) |> join
                        ))
                    ), write_zeros) |>
                    fun(p235 =>
                      zip(p235, transpose(bc._2)) |>
                        mapSeq(fun(p237 =>
                          zip(p237._1, p237._2) |>
                            mapSeq(fun(p64 => (p64._1 * alpha) + (p64._2 * beta)))
                        ))
                    ) |> transpose
                )) |> join |> transpose
            )) |> join
        )
      )
    }

    val keplerBest = {
      val v3: Nat = 128
      val v4: Nat = 4
      val v5: Nat = 8
      val v6: Nat = 64
      val v7: Nat = 8

      def tile: Expr = nFun(s1 => nFun(s2 =>
        map(map(transpose) o split(s2) o transpose) o split(s1) ))

      val zeros = nFun(n1 => nFun(n2 => nFun(n3 => nFun(n4 =>
        generate(fun(IndexType(n4))(_ =>
          generate(fun(IndexType(n3))(_ =>
            generate(fun(IndexType(n2))(_ =>
              generate(fun(IndexType(n1))(_ => l(0.0f)))))))))))))

      def tile2: Expr = nFun(s1 => nFun(s2 => implN(n1 => implN(n2 => fun(ArrayType(n1, ArrayType(n2, float)))(x =>
        transpose (map (transpose) (split (s1) (map (split (s2)) (x))))  )))))

      def redOp: Expr = fun((8`.`32`.`8`.`4`.`float) ->: ( (8`.`64`.`float) x (8`.`128`.`float) ) ->: (8`.`32`.`8`.`4`.`float) )((p14, p15) =>
        p15 |> (fun(p29 =>
          zip (p29._1) (p29._2)
            //TODO? In contrast to old code. This creates a struct of arrays.
            |> toLocalFun(mapLocal(1) (fun(p31 => pair (mapLocal(0) (id) (p31._1)) (mapLocal(0) (id) (p31._2)) )))
            |> unzip
        )) |> fun(p16 =>
          zip (p14) (split (v5) (transpose (p16._1)))
            |> mapLocal(1) (fun(p17 =>
            zip (p17._1) (split (v4) (reorderWithStride (v3/v4) (transpose (p16._2))))
              |> mapLocal(0) (fun(p18 =>
              zip (transpose (p17._2)) (transpose (p18._2))
                |> oclReduceSeq (AddressSpace.Private) (fun( (p20, p21) =>
                //TODO maybe this subexpression could be much cleaner more efficient and express the same
                pair (toPrivate(mapSeq (id) (p21._1))) (toPrivate(mapSeq (id) (p21._2)))
                  |> fun(p22 =>
                    //TODO something goes horribly wrong in the following part.
                    //Two additional loops are generated and unexpected values are being accessed
                    //It looks like CSE or better Let is needed.
                    zip (p20) (p22._1) |> mapSeq (fun(p23 =>
                      zip (p23._1) (p22._2) |> mapSeq (fun(p24 =>
                        p24._1 + (p23._2 * p24._2) )) )) ))) (p18._1 |> mapSeq (mapSeq (fun(x => x))) )
                |> mapSeq (mapSeq (fun(x => x)))
            ))
          ))
        ))

      nFun((n, m, k) =>
        fun((k`.`m`.`float) ->: (k`.`n`.`float) ->: (m`.`n`.`float) ->: float ->: float ->: (m`.`n`.`float))
        ((A, B, C, alpha, beta) =>
          zip (tile2 (v7) (v6) (A)) (tile (v6) (v3) (C))
          |> mapWorkGroup(1)(
            //BEGIN mapWorkGroup(1) Function
            fun(p2 =>
            zip (tile2 (v7) (v3) (B)) (p2._2)
            |> mapWorkGroup(0)(
              //BEGIN mapWorkGroup(0) Function
              fun(p3 =>
              zip (p2._1) (p3._1)
              |> oclReduceSeq (AddressSpace.Private) (redOp)
                (zeros (v4) (v5) (v3 * Cst(1) /^ v4) (v6 * Cst(1) /^ v5)
                  |> mapLocal(1) (mapLocal(0) (mapSeq (mapSeq (id)))))
              //mapSeq was removed because reduce does not wrap reduced results in arrays anymore
              |> fun(x =>
                zip (x) (split (v5) (p3._2))
                  //TODO Should write intermediate results into private mem? What exactly does old Lift code mean here?
                  |> mapLocal(1) (fun(y =>
                  zip (y._1) (split (v4) (reorderWithStride (v3/v4) (transpose (y._2)))) |> mapLocal(0) (fun(z =>
                    zip (z._1) (transpose (z._2)) |> mapSeq (fun(a =>
                      zip (a._1) (a._2) |> mapSeq (fun(x =>
                        (x._1 * alpha) + (x._2 * beta) )))))))))
              |> map (fun(p4 => p4
                |> map (transpose)
                |> join
                |> transpose
                |> map (reorderWithStride (v3 / v4))
              )) |> join |> transpose
            )
            //END mapWorkGroup(0) Function
            ) |> join |> transpose
          )
         //END mapWorkGroup(1) Function
          )  |> join
        ))
    }
  }

  test("Sequential gemm type inference works") {
    infer(sequential)
  }

  test("Sequential gemm compiles to syntactically correct C") {
    gen.CProgram(sequential)
  }

  test("mali gemm type inference works") {
    infer(ocl.mali_GEMM)
  }

  test("mali gemm compiles to syntactically correct kernel") {
    gen.OpenCLKernel(ocl.mali_GEMM)
  }

  test("Kepler best type inference works") {
    infer(ocl.keplerBest)
  }

  test("Kepler best compiles to syntactically correct kernel") {
    gen.OpenCLKernel(LocalSize((16,4,1)), GlobalSize((256, 128, 1)))(ocl.keplerBest(1024)(1024)(1024), "KERNEL")
  }

  test("OpenCL sequential gemm versions produce the expected result") {
    import idealised.OpenCL._
    import scala.util.Random

    val random = new Random()

    val n = 64
    val m = 32
    val k = 64
    val A = Array.fill(m, k)((random.nextInt(10) + 1).toFloat)
    val B = Array.fill(k, n)((random.nextInt(10) + 1).toFloat)
    val C = Array.fill(m, n)((random.nextInt(10) + 1).toFloat)
    val alpha = 2.0f
    val beta = 3.0f

    val gold = matrixMatrixMultiply(A, B, C, alpha, beta)

    val runKernel = gen.OpenCLKernel(ocl.sequential).as[ScalaFunction `(`
      Int `,` Int `,` Int `,`
      Array[Array[Float]] `,`
      Array[Array[Float]] `,`
      Array[Array[Float]] `,`
      Float `,`
      Float `)=>` Array[Float]]
    val (flatOutput, _) = runKernel(LocalSize(1), GlobalSize(1))(n `,` m `,` k `,` A `,` B `,` C `,` alpha `,` beta)
    val output: Array[Array[Float]] = flatOutput.grouped(n).toArray

    (output zip gold).foreach { case (outputRow, goldRow) =>
      assert(outputRow sameElements goldRow)
    }
  }

  test("OpenCL mali_gemm version produces the expected result") {
    import idealised.OpenCL._
    import scala.util.Random

    val random = new Random()

    val n = 512
    val m = 256
    val k = 512
    val A = Array.fill(m, k)(1.0f) //((random.nextInt(10) + 1).toFloat)
    val B = Array.fill(k, n)(1.0f) //((random.nextInt(10) + 1).toFloat)
    val C = Array.fill(m, n)(1.0f) //(((random.nextInt(10) + 1).toFloat)
    val alpha = 1.0f
    val beta = 1.0f

    val gold = matrixMatrixMultiply(A, B, C, alpha, beta)

    val runKernel = gen.OpenCLKernel(ocl.mali_GEMM).as[ScalaFunction `(`
      Int `,` Int `,` Int `,`
      Array[Array[Float]] `,`
      Array[Array[Float]] `,`
      Array[Array[Float]] `,`
      Float `,`
      Float `)=>` Array[Float]]
    val (flatOutput, _) = runKernel(LocalSize((2, 2)), GlobalSize((n/2, n/2)))(n `,` m `,` k `,` A `,` B `,` C `,` alpha `,` beta)

    val output: Array[Array[Float]] = flatOutput.grouped(n).toArray

    (output zip gold).foreach { case (outputRow, goldRow) =>
      assert(outputRow sameElements goldRow)
    }
  }

  ignore("OpenCL keplerBest version produces the expected result") {
    import idealised.OpenCL._
    import scala.util.Random

    val random = new Random()

    val n = 512
    val m = 256
    val k = 512
    val A = Array.fill(m, k)(1.0f) //((random.nextInt(10) + 1).toFloat)
    val B = Array.fill(k, n)(1.0f) //((random.nextInt(10) + 1).toFloat)
    val C = Array.fill(m, n)(1.0f) //(((random.nextInt(10) + 1).toFloat)
    val alpha = 1.0f
    val beta = 1.0f

    val gold = matrixMatrixMultiply(A, B, C, alpha, beta)

    val runKernel = gen.OpenCLKernel(ocl.keplerBest).as[ScalaFunction `(`
      Int `,` Int `,` Int `,`
      Array[Array[Float]] `,`
      Array[Array[Float]] `,`
      Array[Array[Float]] `,`
      Float `,`
      Float `)=>` Array[Float]]
    val (flatOutput, _) = runKernel(LocalSize((2, 2)), GlobalSize((n/2, n/2)))(n `,` m `,` k `,` A `,` B `,` C `,` alpha `,` beta)

    val output: Array[Array[Float]] = flatOutput.grouped(n).toArray

    (output zip gold).foreach { case (outputRow, goldRow) =>
      assert(outputRow sameElements goldRow)
    }
  }
}
