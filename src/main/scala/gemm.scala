
import idealised.DPIA.Phrases.PrettyPhrasePrinter
import idealised.OpenCL._
import idealised.OpenCL.SurfaceLanguage._
import lift.arithmetic._
import idealised.DPIA.Semantics.OperationalSemantics._
import idealised.DPIA._
import idealised.DPIA.Types.{ArrayType, ExpType, TypeInference, float}
import idealised.OpenCL.CodeGenerator
import idealised.OpenCL.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.{Expr, LiteralExpr}
import opencl.executor.Executor

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
  val aT = exp"[$M.$K.$dt]"
  val bT = exp"[$N.$K.$dt]"
  val cT = exp"[$M.$N.$dt]"

  def printOpenCLKernel(name: String,
                        untypedLambda: Expr[ExpType -> (ExpType -> ExpType)]): Unit = {
    val lambda = TypeInference(untypedLambda, Map())
    println(name + ":\n" + PrettyPhrasePrinter(lambda))
    lambda.typeCheck()

    println(s"-- $name --")
    val kernel = CodeGenerator.makeKernel(lambda, localSize = 128, globalSize = N)
    println(kernel.code)

    val fun = kernel.asFunction[(Array[Float] :: Array[Float] :: Nil) =:=> Array[Float]]

    val size = 1024 * 1024

    val xs = Array.fill(size)(Random.nextInt(4).toFloat)
    val ys = Array.fill(size)(Random.nextInt(4).toFloat)

    val (res, time) = fun(xs :: ys)
    println(s"RESULT KERNEL1 NAME: $name TIME: $time")
    if (check) {
      val gold = (xs zip ys).map{ case (x, y) => x * y }.sum
      if (res.sum - gold < epsilon) {
        println(s"Computed result MATCHES with gold solution.")
      } else {
        println(s"ERROR computed result differs from gold solution.")
        println(s"res: ${res.sum} vs. gold: $gold")
      }
    }

    println("----------------\n")
  }

  val id   = λ(x => x)
  val mult = λ(x => λ(a => x * a))
  val add  = λ(x => λ(a => x + a))

  val v3 = 2
  val v4 = 2

  val zeros = LiteralExpr(
    ArrayData(Vector.fill(v4)(ArrayData(Vector.fill(v3)(FloatData(0.0f))))),
    ArrayType(v4, ArrayType(v3, float)))

  val transpose = λ(x => x)
  val dot  = λ(x => x)

  val maliGEMM = λ(aT)(a => λ(bT)(b => λ(cT)(c => λ(ExpType(dt))(alpha => λ(ExpType(dt))(beta =>
    join() o mapGlobal(λ(ac =>
      transpose o map(transpose) o
      transpose o join() o mapGlobal(λ(bc =>
        transpose o toGlobal(
          mapSeq(λ(p235 =>
            mapSeq(λ(p237 =>
              mapSeq(λ(p64 =>
                add(mult(p64._1)(alpha))(mult(p64._2)(beta))
              ), zip(p237._1, p237._2))
            )) $ zip(p235, transpose $ bc._2)
          ))
        ) o
        reduceSeq(λ(p67 => λ(p236 =>
          mapSeq(λ(p54 =>
            join() o mapSeq(λ(p157 =>
              reduceSeq(add, p157._1) o
                mapSeq(dot) $
                zip(asVector(4) $ p54._2, asVector(4) $ p157._2)
            )) $ zip(p54._1, transpose $ p236._2)
          )) $ zip(p67, transpose $ p236._1)
        )), zeros) $
        zip(split(4) o transpose $ ac._1, split(4) o transpose $ bc._1)
      )) $ zip(split(2) $ b, split(2) o transpose $ ac._2)
    )) $ zip(split(2) $ a, split(2) $ c)
  )))))

  {
    val lambda = TypeInference(maliGEMM, Map())
    println("maliGEMM:\n" + PrettyPhrasePrinter(lambda))
    lambda.typeCheck()
  }

}
