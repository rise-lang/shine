
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

import scala.language.implicitConversions
import scala.util.Random

object gemm extends App {

//  Executor.loadLibrary()
//  Executor.init()

  val epsilon = 1.0f

  val check = true
  val K = SizeVar("K")
  val M = SizeVar("M")
  val N = SizeVar("N")
  val dt = float
  val aT = ArrayType(M, ArrayType(K, dt))
  val bT = ArrayType(N, ArrayType(K, dt))
  val cT = ArrayType(M, ArrayType(N, dt))

  def printOpenCLKernel(name: String,
                        untypedLambda: Expr[DataType -> (DataType -> (DataType -> (DataType -> (DataType -> DataType))))]): Unit = {
    val lambda = TypeInference(untypedLambda, Map()).convertToPhrase
    println(name + ":\n" + PrettyPhrasePrinter(lambda))
    lambda.typeCheck()

    println(s"-- $name --")
    val kernel = CodeGenerator.makeKernel(lambda, localSize = 128, globalSize = N)
    println(kernel.code)

//    val fun = kernel.as[ScalaFunction `(` Array[Float] `,` Array[Float] `)=>` Array[Float]]
//
//    val size = 1024 * 1024
//
//    val xs = Array.fill(size)(Random.nextInt(4).toFloat)
//    val ys = Array.fill(size)(Random.nextInt(4).toFloat)
//
//    val (res, time) = fun(xs `,` ys)
//    println(s"RESULT KERNEL1 NAME: $name TIME: $time")
//    if (check) {
//      val gold = (xs zip ys).map{ case (x, y) => x * y }.sum
//      if (res.sum - gold < epsilon) {
//        println(s"Computed result MATCHES with gold solution.")
//      } else {
//        println(s"ERROR computed result differs from gold solution.")
//        println(s"res: ${res.sum} vs. gold: $gold")
//      }
//    }

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
      join() o mapGlobal(λ(ac =>
        transposeW() o join() o
        mapGlobal(λ(bc =>
          transpose() o toGlobal(λ(p235 =>
              mapSeq(λ(p237 =>
                mapSeq(λ(p64 =>
                  add(mult(p64._1)(alpha))(mult(p64._2)(beta))
                ), zip(p237._1, p237._2))
              )) $ zip(p235, transpose() $ bc._2)
          )) o
          reduceSeq(λ(p236 => λ(p67 =>
            mapSeq(λ(p54 =>
              mapSeq(λ(p157 =>
                reduceSeq(add, p157._1) o
                  mapSeq(dot) $
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

}
