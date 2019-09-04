package apps

import idealised.util.gen
import lift.arithmetic.Cst
import lift.core.DSL._
import lift.core.{Expr, Nat}
import lift.core.HighLevelConstructs._
import lift.core.primitives._
import lift.core.types._

//noinspection TypeAnnotation
class gemm extends idealised.util.TestsWithExecutor {

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

      val zeros = implN(n => implN(m =>
        generate(fun(IndexType(m))(_ => generate(fun(IndexType(n))(_ => l(0.0f)))))))


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
                    ), zeros) |>
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

      def id: Expr = fun(x => x)

      def tile2: Expr = nFun(s1 => nFun(s2 => implN(n1 => implN(n2 => fun(ArrayType(n1, ArrayType(n2, float)))(x =>
        transpose (map (transpose) (split (s1) (map (split (s2)) (x))))  )))))

      def redOp: Expr = fun((8`.`32`.`8`.`4`.`float) ->: ( (8`.`64`.`float) x (8`.`128`.`float) ) ->: (8`.`32`.`8`.`4`.`float) )((p14, p15) =>
        p15 |> (fun(p29 =>
          zip (p29._1) (p29._2)
            |> toLocalFun(mapLocal(1) (fun(p31 => pair (mapLocal(0) (id) (p31._1)) (mapLocal(0) (id) (p31._2)) )))
            |> unzip
        )) |> fun(p16 =>
          zip (p14) (split (v5) (transpose (p16._1)))
            |> mapLocal(1) (fun(p17 =>
            zip (p17._1) (split (v4) (reorderWithStride (v3/v4) (transpose (p16._2))))
              |> mapLocal(0) (fun(p18 =>
              zip (transpose (p17._2)) (transpose (p18._2))
                |> oclReduceSeq (AddressSpace.Private) (fun( (p20, p21) =>
                  toPrivate (pair (mapSeq (id) (p21._1)) (mapSeq (id) (p21._2)))
                  |> fun(p22 =>
                    zip (p20) (p22._1) |> mapSeq (fun(p23 =>
                      zip (p23._1) (p22._2) |> mapSeq (fun(p24 =>
                        p24._1 + (p23._2 * p24._2) )) )) ))) (p18._1)
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
                (zeros (v4) (v5) (v3 * Cst(1) /^ v4) (v6 * Cst(1) /^ v5) |> toPrivateFun(mapLocal(1) (mapLocal(0) (mapSeq (mapSeq (id))))))
              //TODO following function shuould eventually write to global
              //mapSeq was removed because reduce does not wrap reduced results in arrays anymore
              |> fun(x =>
                zip (x) (split (v5) (p3._2))
                  //TODO Should write intermediate results into private mem? What exactly does old Lift code mean here?
                  |> mapLocal(1) (fun(y =>
                  zip (y._1) (split (v4) (reorderWithStride (v3/v4) (transpose (y._2)))) |> mapLocal(0) (fun(z =>
                    zip (z._1) (transpose (z._2)) |> mapSeq (fun(a =>
                      zip (a._1) (a._2) |> mapSeq (fun(x =>
                        (x._1 * alpha) + (x._2 * beta) )))))))))
              |> printType
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

//  val keplerBestRev = {
//    val v3: Nat = 128
//    val v4: Nat = 4
//    val v5: Nat = 8
//    val v6: Nat = 64
//    val v7: Nat = 8
//
//    def tile: Expr = nFun(s1 => nFun(s2 =>
//      map(map(transpose) o split(s2) o transpose) o split(s1) ))
//
//    val zeros = nFun(n1 => nFun(n2 => nFun(n3 => nFun(n4 =>
//      generate(fun(IndexType(n4))(_ =>
//        generate(fun(IndexType(n3))(_ =>
//          generate(fun(IndexType(n2))(_ =>
//            generate(fun(IndexType(n1))(_ => l(0.0f)))))))))))))
//
//    def id: Expr = fun(x => x)
//
//    def tile2: Expr = nFun(s1 => nFun(s2 => implN(n1 => implN(n2 => fun(ArrayType(n1, ArrayType(n2, float)))(x =>
//      transpose (map (transpose) (split (s1) (map (split (s2)) (x))))  )))))
//
//    import lift.OpenCL.primitives._
//    val kernel =
//    fun((k`.`m`.`float) ->: (k`.`n`.`float) ->: (m`.`n`.`float) ->: float ->: float ->: (m`.`n`.`float))
//    ((A, B, C, alpha, beta) =>
//      join() $
//        MapWrg(1)(
//          //BEGIN mapWg(1) function
//          fun((p_2) =>
//          (TransposeW(),
//            (Join(),
//              (MapWrg(0)(
//                //BEGIN mapWg(0) function
//                fun((p_3) =>
//                (TransposeW(),
//                  (Join(),
//                    (Map(fun((p_4)
//                    => (Map(fun((p_5)
//                      => (Scatter(ReorderWithStride(v__3 / v__4)), p_5))),
//                        (TransposeW(),
//                          (Join(),
//                            (Map(fun((p_6) =>
//                              (TransposeW(),
//                                (Map(fun((p_7) =>
//                                  (TransposeW(), p_7))),
//                                  (TransposeW(), p_6))))),
//                              (TransposeW(), p_4))))))),
//                      (TransposeW(),
//
//                        (toGlobal(MapSeq(fun(x =>
//                          MapLcl(1)(fun(y =>
//                            MapLcl(0)(fun( z =>
//                              MapSeq(fun(a =>
//                                MapSeq(fun(x =>
//                                  add(
//                                    toPrivate(mult)(Get(x, 0), alpha),
//                                    toPrivate(mult)(Get(x, 1), beta)
//                                  )
//                                )) $ Zip(Get(a, 0), Get(a, 1))
//                              )) $ Zip(Get(z, 0), Transpose() $ Get(z, 1))
//                            )) $ Zip(Get(y, 0), Split(v__4) o ReorderStride(v__3 / v__4) o Transpose() $ Get(y, 1))
//                          )) $ Zip(x, Split(v__5) $ Get(p_3, 1))
//                        ))),
//                          (ReduceSeq(
//                            //BEGIN reduce operator
//                            fun((p_14, p_15) =>
//                            (fun((p_16) =>
//                              (MapLcl(1)(fun((p_17) =>
//                                (Join(),
//                                  (MapLcl(0)(fun((p_18) =>
//                                    (MapSeq(fun((p_19) => p_19)),
//                                      (ReduceSeq(fun((p_20, p_21) =>
//                                        (fun((p_22) =>
//                                          (MapSeq(fun((p_23) =>
//                                            (MapSeq(fun((p_24) =>
//                                              (add,
//                                                (Get(0), p_24),
//                                                (mult, FunCall(Get(1), p_23),
//                                                  (Get(1), p_24))
//                                              ))
//                                            ), (Zip(2),
//                                              (Get(0), p_23),
//                                              (Get(1), p_22))))),
//                                            (Zip(2), p_20,
//                                              (Get(0), p_22)))),
//                                          (toPrivate(fun((p_25) =>
//                                            (fun((p_26) =>
//                                              (Tuple(2),
//                                                (MapSeq(fun((p_27) =>
//                                                  (id, p_27))),
//                                                  (Get(0), p_26)),
//                                                (MapSeq(fun((p_28) =>
//                                                  (id, p_28))),
//                                                  (Get(1), p_26)))), p_25))), p_21)))),
//                                        (Get(0), p_18),
//                                        (Zip(2),
//                                          (Transpose(),
//                                            (Get(1), p_17)),
//                                          (Transpose(),
//                                            (Get(1), p_18))))))),
//                                    (Zip(2),
//                                      (Get(0), p_17),
//                                      (Split(v__4),
//                                        (Gather(ReorderWithStride(v__3 / v__4)),
//                                          (Transpose(),
//                                            (Get(1), p_16))))))))),
//                                (Zip(2), p_14,
//                                  (Split(v__5),
//                                    (Transpose(),
//                                      (Get(0), p_16)))))),
//                              (toLocal(fun((p_29) =>
//                                (fun((p_30) =>
//                                  (Unzip(),
//                                    (MapLcl(1)(fun((p_31) =>
//                                      (Tuple(2),
//                                        (MapLcl(0)(fun((p_32) =>
//                                          (id, p_32))),
//                                          (Get(0), p_31)),
//                                        (MapLcl(0)(fun((p_33) =>
//                                          (id, p_33))),
//                                          (Get(1), p_31))))),
//                                      (Zip(2),
//                                        (Get(0), p_30),
//                                        (Get(1), p_30))))), p_29))), p_15)))),
//                                      //END reduce operator
//
//                            //BEGIN copy accumulator value for reduce
//                            (MapLcl(1)(fun((p_34) =>
//                              (MapLcl(0)(fun((p_35) =>
//                                (MapSeq(fun((p_36) =>
//                                  (MapSeq(fun((p_37) =>
//                                    (id, p_37))), p_36))), p_35))), p_34))),
//                              Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, v__4), v__5), v__3 * 1 /^ v__4), v__6 * 1 /^ v__5))),
//                            //END copy accumulator value for reduce
//
//                            //BEGIN reduce input
//                            (Zip(2), Get(p_2, 0), Get(p_3, 0)))
//                          //END reduce input
//                        )
//
//                      )))))
//                //END mapWg(0) function
//              ),
//                //BEGIN input mapWg(0)
//                Zip((Transpose(),
//                  (Map(fun((p_38) =>
//                    (Transpose(), p_38))),
//                    (Split(v__7),
//                      (Map(fun((p_39) =>
//                        (Split(v__3), p_39))), B)))), Get(p_2, 1))
//                //END input mapWg(0)
//              ))))
//            //END mapWg(1) function
//        ),
//          //BEGIN input mapWg(1)
//          Zip((Transpose(),
//            (Map(fun((p_40) =>
//              (Transpose(), p_40))),
//              (Split(v__7),
//                (Map(fun((p_41) =>
//                  (Split(v__6), p_41))), A)))), Tile(v__6, v__3) $ C))))
//          //END input mapWg(1)
//  }

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
    gen.OpenCLKernel(ocl.keplerBest)
  }

  test("OpenCL sequential gemm versions produce the expected result") {
    import idealised.OpenCL._
    import scala.util.Random

    val random = new Random()

    val n = 512
    val m = 256
    val k = 512
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
    val (flatOutput, _) = runKernel(LocalSize(128), GlobalSize(m))(n `,` m `,` k `,` A `,` B `,` C `,` alpha `,` beta)
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

  test("OpenCL keplerBest version produces the expected result") {
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
