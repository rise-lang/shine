
import idealised.DPIA.Phrases.PrettyPhrasePrinter
import idealised.OpenCL.SurfaceLanguage.DSL._
import idealised.OpenCL._
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Expr
import idealised.SurfaceLanguage._
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage.Semantics._
import lift.arithmetic._
import opencl.executor.Executor

import org.junit.Assert._

import scala.language.implicitConversions
import scala.util.Random

object gemm extends App {

  Executor.loadLibrary()
  Executor.init()

  val epsilon = 1.0f

  val check = true
  val K = SizeVar("K")
  val M = SizeVar("M")
  val N = SizeVar("N")
  val dt = float
  val aT = ArrayType(M, ArrayType(K, dt))
  val bT = ArrayType(N, ArrayType(K, dt))
  val cT = ArrayType(M, ArrayType(N, dt))

  def myPrint(m: Array[Array[Float]]): Unit = {
    m.foreach( r => {
      println(r.map(x => f"$x%2.0f").reduce(_ + " " + _))
    } )
  }

  def myPrint(m: Array[Float]): Unit = {
    println(m.map(x => f"$x%2.0f").reduce(_ + " " + _))
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
                        untypedLambda: Expr[DataType -> (DataType -> (DataType -> (DataType -> (DataType -> DataType))))]): Unit = {
    val lambda = TypeInference(untypedLambda, Map()).convertToPhrase
    println(name + ":\n" + PrettyPhrasePrinter(lambda))
    lambda.typeCheck()

    println(s"-- $name --")
    val kernel = KernelGenerator.makeKernel(lambda, localSize = 8, globalSize = N)
    println(kernel.code)

    val fun = kernel.as[
      ScalaFunction `(`
        Array[Array[Float]] `,`
        Array[Array[Float]] `,`
        Array[Array[Float]] `,`
        Float `,` Float `)=>` Array[Float] ]

    val size = 8

    val A: Array[Array[Float]] = Array.tabulate(size, size)((c, _) => c) // ((r, c) => c * 1.0f + r * 0.5f)
    //((_, _) => (Random.nextInt(5)+1).toFloat)
    val B: Array[Array[Float]] = Array.tabulate(size, size)((_, r) => r) //((r, c) => c * 1.0f + r * 0.5f)
    //((_, _) => (Random.nextInt(5)+1).toFloat)
    val C: Array[Array[Float]] = Array.tabulate(size, size)((_, _) => 1.0f)
    //((_, _) => (Random.nextInt(5)+1).toFloat)
    val alpha = 1.0f //(Random.nextInt(5)+1).toFloat
    val beta = 0.0f //(Random.nextInt(5)+1).toFloat

    println("A")
    myPrint(A)

    println("B")
    myPrint(B)

    println("C")
    myPrint(C)

    println(s"alpha: $alpha")
    println(s"beta: $beta")

    val (res, time) = fun(A `,` B `,` C `,` alpha `,` beta)

    println("res")
    myPrint(res.grouped(size).toArray)

    if (check) {
      val gold = matrixMatrixMultiply(A, B, C, alpha, beta)

      println("gold")
      myPrint(gold)
      assertArrayEquals(gold.flatten, res, 0.0001f)
    }
    println(s"RESULT KERNEL NAME: $name TIME: $time")

    println("----------------\n")
  }

  val mult = λ(x => λ(a => x * a))
  val add  = λ(x => λ(a => x + a))

  val p1 = 2
  val p2 = 2
  val p3 = 4

  val zeros = LiteralExpr(
    ArrayData(Vector.fill(p2)(ArrayData(Vector.fill(p1)(FloatData(0.0f))))),
    ArrayType(p2, ArrayType(p1, float)))

  val dot  = λ(x => oclFun("dot", Seq(float4, float4), float, Seq(x._1, x._2)))

  val maliGEMM =
    λ(aT)(a => λ(bT)(b => λ(cT)(c => λ(dt)(alpha => λ(dt)(beta =>
      join() o mapGlobal(0)(λ(ac =>
//        transposeW() o
        join() o
        mapGlobal(1)(λ(bc =>
//          transposeW() o
          λ(p235 =>
            mapSeq(λ(p237 =>
                mapSeq(λ(p64 =>
                  p64._1
                  //add(mult(p64._1)(alpha))(mult(p64._2)(beta))
                )) $ zip(p237._1, p237._2)
              )) $ zip(p235, transpose() $ bc._2)
          ) o
          reduceSeq(λ(p236 => λ(p67 =>
            mapSeq(λ(p54 =>
              join() o
              mapSeq(λ(p157 =>
                mapSeq(λ(x => p157._1 + dot(x))) $
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

  printOpenCLKernel("maliGEMM", maliGEMM)

//  val maliGEMM_ =
//    λ(aT)(a => λ(bT)(b => λ(cT)(c => λ(dt)(alpha => λ(dt)(beta =>
//      zip(a :>> split(p2),
//          c :>> split(p2) ) :>>
//      mapGlobal(0)(λ(ac =>
//        zip(b :>> split(p2),
//            ac._2 :>> transpose() :>> split(p1) ) :>>
//        mapGlobal(1)(λ(bc =>
//          zip(ac._1 :>> transpose() :>> split(p3),
//              bc._1 :>> transpose() :>> split(p3) ) :>>
//          reduceSeq(λ(p236 => λ(p67 =>
//            zip(p67,
//                p236._1 :>> transpose() ) :>>
//            mapSeq(λ(p54 =>
//              zip(p54._1,
//                  p236._2 :>> transpose() ) :>>
//              mapSeq(λ(p157 =>
//                zip(p54._2 :>> asVector(4),
//                    p157._2 :>> asVector(4) ) :>>
//                mapSeq(λ(x => p157._1 + dot(x)))
//              )) :>> join()))
//          )), zeros) :>>
//          λ(p235 =>
//            zip(p235, bc._2 :>> transpose()) :>>
//            mapSeq(λ(p237 =>
//              zip(p237._1, p237._2) :>>
//              mapSeq(λ(p64 =>
//                add(mult(p64._1)(alpha))(mult(p64._2)(beta))))))
//          ) :>> transpose()
//        )) :>> join() :>> transposeW()
//      )) :>> join()
//    )))))
//
//  printOpenCLKernel("maliGEMM_", maliGEMM_)
}
