
import idealised.DPIA.Phrases.PrettyPhrasePrinter
import idealised.DPIA.Types.TypeCheck
import idealised.OpenCL.SurfaceLanguage.DSL._
import idealised.OpenCL._
import idealised.OpenMP
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Semantics._
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.{Expr, _}
import lift.arithmetic._

import scala.language.{implicitConversions, reflectiveCalls}

object gemm extends App {

//  Executor.loadLibrary()
//  Executor.init()

  val epsilon = 1.0f

  val check = true
  val K = SizeVar("K")
  val M = SizeVar("M")
  val N = SizeVar("N")

  def myPrint(m: Array[Array[Float]]): Unit = {
    m.foreach( r => {
      println(r.map(x => f"$x%4.0f").reduce(_ + " " + _))
    } )
  }

  def myPrint(m: Array[Float]): Unit = {
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

  def printOpenCLKernel(name: String,
                        untypedLambda: Expr): Unit = {
    val lambda = TypeInference(untypedLambda, Map()).convertToPhrase
    println(name + ":\n" + PrettyPhrasePrinter(lambda))
    TypeCheck(lambda)

    println(s"-- $name --")
    val kernel = KernelGenerator.makeCode(localSize = 8, globalSize = N)(lambda)
    println(kernel.code)

//    val fun = kernel.as[
//      ScalaFunction `(`
//        Array[Array[Float]] `,`
//        Array[Array[Float]] `,`
//        Array[Array[Float]] `,`
//        Float `,` Float `)=>` Array[Float] ]
//
//    val size = 8
//
//    val A: Array[Array[Float]] = Array.tabulate(size, size)((c, _) => c + 1.0f) // ((r, c) => c * 1.0f + r * 0.5f)
//    //((_, _) => (Random.nextInt(5)+1).toFloat)
//    val B: Array[Array[Float]] = Array.tabulate(size, size)((_, r) => r + 1.5f) //((r, c) => c * 1.0f + r * 0.5f)
//    //((_, _) => (Random.nextInt(5)+1).toFloat)
//    val C: Array[Array[Float]] = Array.tabulate(size, size)((_, _) => 1.0f)
//    //((_, _) => (Random.nextInt(5)+1).toFloat)
//    val alpha = 1.0f //(Random.nextInt(5)+1).toFloat
//    val beta = 0.0f //(Random.nextInt(5)+1).toFloat
//
//    println(s"alpha: $alpha")
//    println(s"beta: $beta")
//
//    val (res, time) = fun(A `,` B.transpose `,` C `,` alpha `,` beta)
//
//    println("res")
//    myPrint(res.grouped(size).toArray)
//
//    if (check) {
//      val gold = matrixMatrixMultiply(A, B, C, alpha, beta)
//
//      println("gold")
//      myPrint(gold)
//      assertArrayEquals(gold.flatten, res, 0.0001f)
//    }
//    println(s"RESULT KERNEL NAME: $name TIME: $time")
//
//    println("----------------\n")
  }

  val mult = fun(x => fun(a => x * a))
  val add  = fun(x => fun(a => x + a))
  val id  = fun(x => x)

  val p1 = 2
  val p2 = 2
  val p3 = 4

  val zeros = LiteralExpr(
    ArrayData(Vector.fill(p2)(ArrayData(Vector.fill(p1)(FloatData(0.0f))))))

  val dot  = fun(x => oclFun("dot", Seq(float4, float4), float, Seq(x._1, x._2)))
  val dotFF = fun(x => foreignFun(float, "dot", Seq((float4, "x"), (float4, "y")),
    "{ return x.data[0]*y.data[0] + x.data[1]*y.data[1] + x.data[2]*y.data[2] + x.data[3]*y.data[3]; }", Seq(x._1, x._2)))

  val maliGEMM =
    fun(ArrayType(M, ArrayType(K, float)))(a =>
      fun(ArrayType(N, ArrayType(K, float)))(b =>
        fun(ArrayType(M, ArrayType(N, float)))(c =>
          fun(float)(alpha =>
            fun(float)(beta =>

      join() o mapGlobal(0)(fun(ac =>
        transpose() o
        join() o
        mapGlobal(1)(fun(bc =>
          transpose() o
          fun(p235 =>
            mapSeq(fun(p237 =>
                mapSeq(fun(p64 =>
                  (p64._1 * alpha) + (p64._2 * beta)
                )) $ zip(p237._1, p237._2)
              )) $ zip(p235, transpose() $ bc._2)
          ) o
          reduceSeq(fun(p236 => fun(p67 =>
            mapSeq(fun(p54 =>
              join() o
              mapSeq(fun(p157 =>
                mapSeq(fun(x => p157._1 + dot(x))) $
                  zip(asVector(4) $ p54._2, asVector(4) $ p157._2)
              )) $ zip(p54._1, transpose() $ p236._2)
            )) $ zip(p67, transpose() $  p236._1)
          )), zeros) $
          zip(
            split(p3) o transpose() $  ac._1,
            split(p3) o transpose() $  bc._1)
        )) $ zip(split(p1) $ b, split(p1) o transpose() $ ac._2)
      )) $ zip(split(p2) $ a, split(p2) $ c)
    )))))

//  printOpenCLKernel("maliGEMM", maliGEMM)

  val maliGEMM_ =
    fun(ArrayType(M, ArrayType(K, float)))(a =>
      fun(ArrayType(N, ArrayType(K, float)))(b =>
        fun(ArrayType(M, ArrayType(N, float)))(c =>
          fun(float)(alpha =>
            fun(float)(beta =>

      zip(a :>> split(p2),
          c :>> split(p2) ) :>>
      mapGlobal(0)(fun(ac =>
        zip(b :>> split(p2),
            ac._2 :>> transpose() :>> split(p1) ) :>>
        mapGlobal(1)(fun(bc =>
          zip(ac._1 :>> transpose() :>> split(p3),
              bc._1 :>> transpose() :>> split(p3) ) :>>
          reduceSeq(fun(p236 => fun(p67 =>
            zip(p67,
                p236._1 :>> transpose() ) :>>
            mapSeq(fun(p54 =>
              zip(p54._1,
                  p236._2 :>> transpose() ) :>>
              mapSeq(fun(p157 =>
                zip(p54._2 :>> asVector(4),
                    p157._2 :>> asVector(4) ) :>>
                mapSeq(fun(x => p157._1 + dot(x)))
              )) :>> join()))
          )), zeros) :>>
          fun(p235 =>
            zip(p235, bc._2 :>> transpose()) :>>
            mapSeq(fun(p237 =>
              zip(p237._1, p237._2) :>>
              mapSeq(fun(p64 =>
                (p64._1 * alpha) + (p64._2 * beta)))))
          ) :>> transpose()
        )) :>> join() :>> transpose()
      )) :>> join()
    )))))

  printOpenCLKernel("maliGEMM_", maliGEMM_)

  {
    import idealised.OpenMP.SurfaceLanguage.DSL._

    val maliGEMM =
      fun(ArrayType(M, ArrayType(K, float)))(a =>
        fun(ArrayType(N, ArrayType(K, float)))(b =>
          fun(ArrayType(M, ArrayType(N, float)))(c =>
            fun(float)(alpha =>
              fun(float)(beta =>

                zip(a :>> split(p2),
                    c :>> split(p2) ) :>>
                  mapPar(fun(ac =>
                    zip(b :>> split(p2),
                        ac._2 :>> transpose() :>> split(p1) ) :>>
                      mapPar(fun(bc =>
                        zip(ac._1 :>> transpose() :>> split(p3),
                            bc._1 :>> transpose() :>> split(p3) ) :>>
                          reduceSeq(fun(p236 => fun(p67 =>
                            zip(p67,
                                p236._1 :>> transpose() ) :>>
                              mapSeq(fun(p54 =>
                                zip(p54._1,
                                    p236._2 :>> transpose() ) :>>
                                  mapSeq(fun(p157 =>
                                    zip(p54._2 :>> asVector(4),
                                        p157._2 :>> asVector(4) ) :>>
                                      mapSeq(fun(x => p157._1 + dotFF(x)))
                                  )) :>> join()))
                          )), zeros) :>>
                          fun(p235 =>
                            zip(p235, bc._2 :>> transpose()) :>>
                              mapSeq(fun(p237 =>
                                zip(p237._1, p237._2) :>>
                                  mapSeq(fun(p64 =>
                                    (p64._1 * alpha) + (p64._2 * beta)))))
                          ) :>> transpose()
                      )) :>> join() :>> transpose()
                  )) :>> join()
              )))))

    val phrase = TypeInference(maliGEMM, Map()).toPhrase
    val program = OpenMP.ProgramGenerator.makeCode(phrase)
    println(program.code)
  }

//  {
//    val v3 = 128
//    val v4 = 4
//    val v5 = 8
//    val v6 = 64
//    val v7 = 8
//
//    def tile(x: Nat, y: Nat): Expr =
//      map(map(transpose()) o split(y) o transpose()) o split(x)
//
//    val zeros = LiteralExpr(
//      ArrayData(Vector.fill(v6 * 1 / v5)(
//        ArrayData(Vector.fill(v3 * 1 / v4)(
//          ArrayData(Vector.fill(v5)(
//            ArrayData(Vector.fill(v4)(
//              FloatData(0.0f))))))))),
//      ArrayType(v6 * Cst(1) /^ v5,
//        ArrayType(v3 * Cst(1) /^ v4,
//          ArrayType(v5,
//            ArrayType(v4,
//              float)))))
//
//    val keplerBest =
//      λ(ArrayType(K, ArrayType(M, float)))(a =>
//        λ(ArrayType(K, ArrayType(N, float)))(b =>
//          λ(ArrayType(M, ArrayType(N, float)))(c =>
//            λ(float)(alpha =>
//              λ(float)(beta =>
//
//      zip(a :>> map(split(v6)) :>> split(v7) :>> map(transpose()) :>> transpose(),
//          c :>> tile(v6, v3)) :>>
//        mapWorkgroup(1)(λ(p_2 =>
//          zip(b :>> map(split(v3)) :>> split(v7) :>> map(transpose()) :>> transpose(),
//              p_2._2) :>>
//            mapWorkgroup(0)(λ(p_3 =>
//              zip(p_2._1, p_3._1) :>>
//              reduceSeq(λ(p_15 => λ(p_14 =>
//                  zip(p_14,
//                      p_15._1 :>> toLocal(mapLocal(1)(mapLocal(0)(id))) :>> transpose() :>> split(v5)) :>>
//                  mapLocal(1)(λ(p_17 =>
//                    zip(p_17._1,
//                      p_15._2 :>> toLocal(mapLocal(1)(mapLocal(0)(id))) :>> transpose() :>>
//                          gather(reorderWithStridePhrase(v3 / v4)) :>> split(v4)) :>>
//                    mapLocal(0)(λ(p_18 =>
//                      zip(p_17._2 :>> transpose(), p_18._2 :>> transpose()) :>>
//                      reduceSeq(λ(p_21 => λ(p_20 =>
//                        p_21 :>> toPrivate(λ(p_25 =>
//                          tuple(p_25._1 :>> mapSeq(id),
//                                p_25._2 :>> mapSeq(id))
//                        )) :>>
//                        λ(p_22 =>
//                          zip(p_20, p_22._1) :>>
//                          mapSeq(λ(p_23 =>
//                            zip(p_23._1, p_22._2) :>>
//                            mapSeq(λ(p_24 =>
//                              p_24._1 + (p_23._2 * p_24._2)))))
//                        ))),
//                        p_18._1)))))
//                )),
//                zeros :>> mapLocal(1)(mapLocal(0)(mapSeq(mapSeq(λ(x => x)))))
//              ) :>>
//              toGlobal(λ(x =>
//                zip(x, p_3._2 :>> split(v5)) :>>
//                mapLocal(1)(λ(y =>
//                  zip(y._1, y._2 :>> transpose() :>> gather(reorderWithStridePhrase(v3 / v4)) :>> split(4)) :>>
//                  mapLocal(0)(λ(z =>
//                    zip(z._1, z._2 :>> transpose()) :>>
//                    mapSeq(λ(a =>
//                      zip(a._1, a._2) :>>
//                      mapSeq(λ(b =>
//                        toPrivate(mult(b._1))(alpha) + toPrivate(mult(b._2))(beta) )))))))))) :>>
//              map(transposeW() >>> map(join() >>> scatter(reorderWithStridePhrase(v3 / v4)))) :>>
//              join() :>> transposeW()
//            )) :>> join() :>> transposeW()
//        )) :>> join() :>> printType("output")
//
//      )))))
//
//    printOpenCLKernel("keplerBest", keplerBest)
//  }

//  {
//    val v3 = 128
//    val v4 = 4
//    val v5 = 8
//    val v6 = 64
//    val v7 = 8
//
//    def tile(x: Nat, y: Nat): Expr =
//      map(map(transpose()) o split(y) o transpose()) o split(x)
//
//    val zeros = LiteralExpr(
//      ArrayData(Vector.fill(v6 * 1 / v5)(
//        ArrayData(Vector.fill(v3 * 1 / v4)(
//          ArrayData(Vector.fill(v5)(
//            ArrayData(Vector.fill(v4)(
//              FloatData(0.0f))))))))),
//      ArrayType(v6 * Cst(1) /^ v5,
//        ArrayType(v3 * Cst(1) /^ v4,
//          ArrayType(v5,
//            ArrayType(v4,
//              float)))))
//
//    val keplerBest =
//      λ(ArrayType(K, ArrayType(M, float)))(a =>
//        λ(ArrayType(K, ArrayType(N, float)))(b =>
//          λ(ArrayType(M, ArrayType(N, float)))(c =>
//            λ(float)(alpha =>
//              λ(float)(beta =>
//
//                zip(a :>> map(split(v6)) :>> split(v7) :>> map(transpose()) :>> transpose(),
//                  c :>> tile(v6, v3)) :>>
//                  mapWorkgroup(1)(λ(p_2 =>
//                    zip(b :>> map(split(v3)) :>> split(v7) :>> map(transpose()) :>> transpose(),
//                      p_2._2) :>>
//                      mapWorkgroup(0)(λ(p_3 =>
//                        zip(p_2._1, p_3._1) :>>
//                          reduceSeq(λ(p_15 => λ(p_14 =>
//                            p_15 :>> toLocal(λ(p_30 =>
//                              zip(p_30._1, p_30._2) :>>
//                                mapLocal(1)(λ(p_31 =>
//                                  tuple(p_31._1 :>> mapLocal(0)(id),
//                                    p_31._2 :>> mapLocal(0)(id)))) :>>
//                                unzip()
//                            )) :>>
//                              λ(p_16 =>
//                                zip(p_14,
//                                  p_16._1 :>> transpose() :>> split(v5)) :>>
//                                  mapLocal(1)(λ(p_17 =>
//                                    zip(p_17._1,
//                                      p_16._2 :>> transpose() :>>
//                                        gather(reorderWithStridePhrase(v3 / v4)) :>> split(v4)) :>>
//                                      mapLocal(0)(λ(p_18 =>
//                                        zip(p_17._2 :>> transpose(), p_18._2 :>> transpose()) :>>
//                                          reduceSeq(λ(p_21 => λ(p_20 =>
//                                            p_21 :>> toPrivate(λ(p_25 =>
//                                              tuple(p_25._1 :>> mapSeq(id),
//                                                p_25._2 :>> mapSeq(id))
//                                            )) :>>
//                                              λ(p_22 =>
//                                                zip(p_20, p_22._1) :>>
//                                                  mapSeq(λ(p_23 =>
//                                                    zip(p_23._1, p_22._2) :>>
//                                                      mapSeq(λ(p_24 =>
//                                                        p_24._1 + (p_23._2 * p_24._2)))))
//                                              ))),
//                                            p_18._1)))))
//                              ))),
//                            zeros :>> mapLocal(1)(mapLocal(0)(mapSeq(mapSeq(λ(x => x)))))
//                          ) :>>
//                          toGlobal(λ(x =>
//                            zip(x, p_3._2 :>> split(v5)) :>>
//                              mapLocal(1)(λ(y =>
//                                zip(y._1, y._2 :>> transpose() :>> gather(reorderWithStridePhrase(v3 / v4)) :>> split(4)) :>>
//                                  mapLocal(0)(λ(z =>
//                                    zip(z._1, z._2 :>> transpose()) :>>
//                                      mapSeq(λ(a =>
//                                        zip(a._1, a._2) :>>
//                                          mapSeq(λ(b =>
//                                            toPrivate(mult(b._1))(alpha) + toPrivate(mult(b._2))(beta) )))))))))) :>>
//                          map(transposeW() >>> map(join() >>> scatter(reorderWithStridePhrase(v3 / v4)))) :>>
//                          join() :>> transposeW()
//                      )) :>> join() :>> transposeW()
//                  )) :>> join() :>> printType("output")
//
//              )))))
//
//    printOpenCLKernel("keplerBest", keplerBest)
//  }
}
