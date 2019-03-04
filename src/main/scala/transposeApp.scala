import idealised.DPIA.Phrases.PrettyPhrasePrinter
import idealised.DPIA.Types.TypeCheck
import idealised.OpenCL._
import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.Types.{ArrayType, DataType, TypeInference, float}
import idealised.SurfaceLanguage.{->, Expr}
import lift.arithmetic.SizeVar
import org.junit.Assert.assertArrayEquals

import scala.language.postfixOps

object transposeApp extends App {

  def myPrint(m: Array[Array[Float]]): Unit = {
    m.foreach( r => {
      println(r.map(x => f"$x%2.0f").reduce(_ + " " + _))
    } )
  }

  def myPrint(m: Array[Float]): Unit = {
    println(m.map(x => f"$x%2.0f").reduce(_ + " " + _))
  }

//  Executor.loadLibrary()
//  Executor.init()

  val Nsize = 12
  val Msize = 8

  def printOpenCLKernel(name: String,
                        untypedLambda: Expr[DataType -> DataType]): Unit = {
    val lambda = TypeInference(untypedLambda, Map()).convertToPhrase
    println(name + ":\n" + PrettyPhrasePrinter(lambda))
    TypeCheck(lambda)

    println(s"Type: ${lambda.t}")

    println(s"-- $name --")
    val kernel = KernelGenerator.makeCode(lambda, localSize = 1, globalSize = 1)
    println(kernel.code)

    val fun = kernel.as[ScalaFunction `(` Array[Array[Float]] `)=>` Array[Float] ]

    val matrix = Array.tabulate(Nsize, Msize)((r, c) => c * 1.0f + r * 8.0f)
    val gold   = matrix.transpose

    println("matrix:")
    myPrint(matrix)
    println("")

    val (res, time) = fun(matrix `;`)

    println("res:")
    myPrint(res.grouped(Nsize).toArray)
    println("")

    println("gold:")
    myPrint(gold)
    println("")

    println(s"RESULT KERNEL1 NAME: $name TIME: $time")
    assertArrayEquals(gold.flatten, res, 0.01f)
    println("----------------\n")
  }

  val N = SizeVar("N")
  val M = SizeVar("M")

  val p = fun(ArrayType(N, ArrayType(M, float)))(x =>
//    join(x)
//   split(M, join(x))
//   join(
     transpose(x)
//   )
//   x
   )

  printOpenCLKernel("transpose", p)

}
