package idealised.util

import idealised.OpenCL.Kernel
import idealised.SurfaceLanguage.{->, Expr}
import idealised.SurfaceLanguage.Types.{DataType, TypeInference}
import lift.arithmetic.ArithExpr

import scala.util.Random

sealed trait Correctness {
  final def isCorrect:Boolean = this == Correct

  def check():Unit = {
    println("Correctness check:")
    this match {
      case Correct => println("Correct!")
      case Wrong(wrongValue, wrongSize) =>
        if(wrongValue) {
          println("Value is wrong!")
        }
        if(wrongSize) {
          println("Size is wrong!")
        }
    }
    assert(this.isCorrect)
  }
}
case object Correct extends Correctness
final case class Wrong(wrongValue:Boolean, wrongSize:Boolean) extends Correctness

object Correctness {
  def apply(kernelOutput:Array[Float], scalaOutput:Array[Float]):Correctness = {
    if(kernelOutput.length == scalaOutput.length) {
      if(isSame(kernelOutput, scalaOutput)) Correct else Wrong(wrongValue = true, wrongSize = false)
    } else {
      val (kOut, sOut) = matchSize(kernelOutput, scalaOutput)
      val valueCorrect = isSame(kOut, sOut)
      Wrong(wrongValue = !valueCorrect, wrongSize = true)
    }
  }

  private def isSame(a:Array[Float], b:Array[Float]):Boolean = {
    a.zip(b).forall{case (x,y) => Math.abs(x - y) < 0.01}
  }

  private def matchSize(a:Array[Float], b:Array[Float]):(Array[Float], Array[Float]) = {
    if(a.length > b.length) (a.take(b.length), b) else (a, b.take(a.length))
  }
}


trait OpenCLAlgorithm {
  //The Scala type representing the input data
  type Input
  //The scala type representing the final result of running the algorithm
  type Result

  private val verbose:Boolean = true

  def dpiaProgram:Expr[DataType -> DataType]

  protected def makeInput(random:Random):Input

  def makeOutput(localSize:Int, globalSize:Int, code:String, runtimeMs:Double, correctness: Correctness):Result

  protected def runScalaProgram(input:Input):Array[Float]

  private def compile(localSize:ArithExpr, globalSize:ArithExpr):Kernel = {
    val kernel = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(this.dpiaProgram, Map()).toPhrase, localSize, globalSize)

    if(verbose) {
      println(kernel.code)
    }
    kernel
  }

  final def run(localSize:Int, globalSize:Int):Result = {
    opencl.executor.Executor.loadAndInit()

    val rand = new Random()
    val input = makeInput(rand)
    val scalaOutput = runScalaProgram(input)

    import idealised.OpenCL.{ScalaFunction, `(`, `)=>`, _}

    val kernel = this.compile(localSize, globalSize)
    val kernelFun = kernel.as[ScalaFunction`(`Input`)=>`Array[Float]]
    val (kernelOutput, time) = kernelFun(input `;`)

    opencl.executor.Executor.shutdown()

    val correct = Correctness(kernelOutput, scalaOutput)

    makeOutput(localSize, globalSize, kernel.code, time.value, correct)
  }
}